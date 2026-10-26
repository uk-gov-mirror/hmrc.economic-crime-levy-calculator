/*
 * Copyright 2024 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.economiccrimelevycalculator.services

import uk.gov.hmrc.economiccrimelevycalculator.config.AppConfig
import uk.gov.hmrc.economiccrimelevycalculator.models.Band.*
import uk.gov.hmrc.economiccrimelevycalculator.models.{Band, BandRange, Bands, CalculateLiabilityRequest, CalculatedLiability, EclAmount}

import javax.inject.Inject

class CalculateLiabilityService @Inject() (appConfig: AppConfig) {

  private val NewBandsStartYear = 2026

  private def requiredBandRange(
    bandRange: Option[BandRange],
    bandName: String,
    year: Int
  ): BandRange =
    bandRange.getOrElse {
      throw new IllegalStateException(
        s"$bandName band configuration is missing for tax year $year"
      )
    }

  private def calculateBand(
    relevantApLength: Int,
    ukRevenue: Long,
    amlRegulatedActivityLength: Int,
    year: Int
  ): (Bands, Band) = {

    val defaultBands = appConfig.defaultBands(year)

    val smallBand =
      defaultBands.small.apportion(
        relevantApLength,
        amlRegulatedActivityLength
      )

    if (year >= NewBandsStartYear) {
      calculateNewBands(
        defaultBands,
        smallBand,
        relevantApLength,
        ukRevenue,
        amlRegulatedActivityLength,
        year
      )
    } else {
      calculateLegacyBands(
        defaultBands,
        smallBand,
        relevantApLength,
        ukRevenue,
        amlRegulatedActivityLength,
        year
      )
    }
  }

  private def calculateLegacyBands(
    defaultBands: Bands,
    smallBand: BandRange,
    relevantApLength: Int,
    ukRevenue: Long,
    amlRegulatedActivityLength: Int,
    year: Int
  ): (Bands, Band) = {

    val defaultMedium =
      requiredBandRange(defaultBands.medium, "Medium", year)

    val defaultLarge =
      requiredBandRange(defaultBands.large, "Large", year)

    val defaultVeryLarge =
      requiredBandRange(defaultBands.veryLarge, "VeryLarge", year)

    val mediumBand =
      defaultMedium.apportion(
        relevantApLength,
        amlRegulatedActivityLength
      )

    val largeBand =
      defaultLarge.apportion(
        relevantApLength,
        amlRegulatedActivityLength
      )

    val veryLargeBand =
      defaultVeryLarge
        .apportion(
          relevantApLength,
          amlRegulatedActivityLength
        )
        .copy(to = defaultVeryLarge.to)

    val bands = Bands(
      small = smallBand,
      medium = Some(mediumBand),
      large = Some(largeBand),
      veryLarge = Some(veryLargeBand),
      apportioned = smallBand.apportioned(defaultBands.small) ||
        mediumBand.apportioned(defaultMedium) ||
        largeBand.apportioned(defaultLarge) ||
        veryLargeBand.apportioned(defaultVeryLarge)
    )

    val calculatedBand =
      ukRevenue match {
        case revenue
            if revenue >= smallBand.from &&
              revenue < smallBand.to =>
          Band.Small

        case revenue
            if revenue >= mediumBand.from &&
              revenue < mediumBand.to =>
          Band.Medium

        case revenue
            if revenue >= largeBand.from &&
              revenue < largeBand.to =>
          Band.Large

        case revenue if revenue >= veryLargeBand.from =>
          Band.VeryLarge

        case _ =>
          throw new IllegalStateException(
            s"Revenue $ukRevenue does not fall into any band for tax year $year"
          )
      }

    bands -> calculatedBand
  }

  private def calculateNewBands(
    defaultBands: Bands,
    smallBand: BandRange,
    relevantApLength: Int,
    ukRevenue: Long,
    amlRegulatedActivityLength: Int,
    year: Int
  ): (Bands, Band) = {

    val defaultA = requiredBandRange(defaultBands.a, "A", year)

    val defaultB = requiredBandRange(defaultBands.b, "B", year)

    val defaultC = requiredBandRange(defaultBands.c, "C", year)

    val defaultD = requiredBandRange(defaultBands.d, "D", year)

    val aBand =
      defaultA.apportion(
        relevantApLength,
        amlRegulatedActivityLength
      )

    val bBand =
      defaultB.apportion(
        relevantApLength,
        amlRegulatedActivityLength
      )

    val cBand =
      defaultC.apportion(
        relevantApLength,
        amlRegulatedActivityLength
      )

    val dBand =
      defaultD
        .apportion(
          relevantApLength,
          amlRegulatedActivityLength
        )
        .copy(to = defaultD.to)

    val bands = Bands(
      small = smallBand,
      a = Some(aBand),
      b = Some(bBand),
      c = Some(cBand),
      d = Some(dBand),
      apportioned = smallBand.apportioned(defaultBands.small) ||
        aBand.apportioned(defaultA) ||
        bBand.apportioned(defaultB) ||
        cBand.apportioned(defaultC) ||
        dBand.apportioned(defaultD)
    )

    val calculatedBand =
      ukRevenue match {
        case revenue
            if revenue >= smallBand.from &&
              revenue < smallBand.to =>
          Band.Small

        case revenue
            if revenue >= aBand.from &&
              revenue < aBand.to =>
          Band.A

        case revenue
            if revenue >= bBand.from &&
              revenue < bBand.to =>
          Band.B

        case revenue
            if revenue >= cBand.from &&
              revenue < cBand.to =>
          Band.C

        case revenue if revenue >= dBand.from =>
          Band.D

        case _ =>
          throw new IllegalStateException(
            s"Revenue $ukRevenue does not fall into any band for tax year $year"
          )
      }

    bands -> calculatedBand
  }

  private[services] def determineAmountDue(
    band: Band,
    bands: Bands,
    year: Int
  ): EclAmount = {

    val defaultBands = appConfig.defaultBands(year)

    band match {
      case Band.Small =>
        createEclAmount(
          calculatedBand = bands.small,
          defaultBand = defaultBands.small
        )

      case Band.Medium =>
        createEclAmount(
          calculatedBand = requiredBandRange(bands.medium, "Medium", year),
          defaultBand = requiredBandRange(defaultBands.medium, "Medium", year)
        )

      case Band.Large =>
        createEclAmount(
          calculatedBand = requiredBandRange(bands.large, "Large", year),
          defaultBand = requiredBandRange(defaultBands.large, "Large", year)
        )

      case Band.VeryLarge =>
        createEclAmount(
          calculatedBand = requiredBandRange(bands.veryLarge, "VeryLarge", year),
          defaultBand = requiredBandRange(defaultBands.veryLarge, "VeryLarge", year)
        )

      case Band.A =>
        createEclAmount(
          calculatedBand = requiredBandRange(bands.a, "A", year),
          defaultBand = requiredBandRange(defaultBands.a, "A", year)
        )

      case Band.B =>
        createEclAmount(
          calculatedBand = requiredBandRange(bands.b, "B", year),
          defaultBand = requiredBandRange(defaultBands.b, "B", year)
        )

      case Band.C =>
        createEclAmount(
          calculatedBand = requiredBandRange(bands.c, "C", year),
          defaultBand = requiredBandRange(defaultBands.c, "C", year)
        )

      case Band.D =>
        createEclAmount(
          calculatedBand = requiredBandRange(bands.d, "D", year),
          defaultBand = requiredBandRange(defaultBands.d, "D", year)
        )
    }
  }

  private def createEclAmount(
    calculatedBand: BandRange,
    defaultBand: BandRange
  ): EclAmount =
    EclAmount(
      amount = calculatedBand.amount,
      apportioned = calculatedBand.amount != defaultBand.amount
    )

  def calculateLiability(calculateLiabilityRequest: CalculateLiabilityRequest): CalculatedLiability = {
    val (bands, band) = calculateBand(
      calculateLiabilityRequest.relevantApLength,
      calculateLiabilityRequest.ukRevenue,
      calculateLiabilityRequest.amlRegulatedActivityLength,
      calculateLiabilityRequest.year
    )

    val amountDue = determineAmountDue(band, bands, calculateLiabilityRequest.year)

    CalculatedLiability(amountDue, bands, band)
  }

}
