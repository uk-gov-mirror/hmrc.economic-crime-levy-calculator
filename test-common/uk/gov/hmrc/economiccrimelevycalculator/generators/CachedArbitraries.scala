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

package uk.gov.hmrc.economiccrimelevycalculator.generators

import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.economiccrimelevycalculator.EclTestData
import uk.gov.hmrc.economiccrimelevycalculator.models._

object CachedArbitraries extends EclTestData {

  implicit lazy val arbBand: Arbitrary[Band] =
    Arbitrary(Gen.oneOf(Band.Small, Band.Medium, Band.Large, Band.VeryLarge))

  implicit lazy val arbBandRange: Arbitrary[BandRange] = Arbitrary {
    for {
      from   <- Gen.posNum[Long]
      to     <- Gen.posNum[Long]
      amount <- Gen.posNum[Double].map(BigDecimal.apply)
    } yield BandRange(from, to, amount)
  }

  implicit lazy val arbEclAmount: Arbitrary[EclAmount] = Arbitrary {
    for {
      amount      <- Gen.posNum[Double].map(d => BigDecimal(d).setScale(2, scala.math.BigDecimal.RoundingMode.DOWN))
      apportioned <- Gen.oneOf(true, false)
    } yield EclAmount(amount, apportioned)
  }

  implicit lazy val arbBands: Arbitrary[Bands] = Arbitrary {
    for {
      small       <- arbBandRange.arbitrary
      medium      <- arbBandRange.arbitrary
      large       <- arbBandRange.arbitrary
      veryLarge   <- arbBandRange.arbitrary
      apportioned <- Gen.oneOf(true, false)
    } yield Bands(small, Some(medium), Some(large), Some(veryLarge), apportioned = apportioned)
  }

  implicit lazy val arbNewBands: Arbitrary[Bands] = Arbitrary {
    for {
      small       <- arbBandRange.arbitrary
      a           <- arbBandRange.arbitrary
      b           <- arbBandRange.arbitrary
      c           <- arbBandRange.arbitrary
      d           <- arbBandRange.arbitrary
      apportioned <- Gen.oneOf(true, false)
    } yield Bands(small = small, a = Some(a), b = Some(b), c = Some(c), d = Some(d), apportioned = apportioned)
  }

  implicit lazy val arbCalculatedLiability: Arbitrary[CalculatedLiability] = Arbitrary {
    for {
      amountDue      <- arbEclAmount.arbitrary
      bands          <- arbBands.arbitrary
      calculatedBand <- arbBand.arbitrary
    } yield CalculatedLiability(amountDue, bands, calculatedBand)
  }

  implicit lazy val arbCalculateLiabilityRequest: Arbitrary[CalculateLiabilityRequest] = Arbitrary {
    for {
      amlRegulatedActivityLength <- Gen.posNum[Int]
      relevantApLength           <- Gen.posNum[Int]
      ukRevenue                  <- Gen.posNum[Long]
      year                       <- Gen.choose(2020, 2050)
    } yield CalculateLiabilityRequest(amlRegulatedActivityLength, relevantApLength, ukRevenue, year)
  }

}
