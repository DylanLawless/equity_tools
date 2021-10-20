# Equity tools

Methods for statistics-based finance management

## Home buyer 
* [home_buyer](home_buyer/)

This repository shows example dataset for:
* 2 persons 
* with modest salaries
* liquid cash savings in less than 5 years sufficient to buy a home
* calculated income/expenses for 3-10years, sufficient up to 1'000'000 CHF.
* includes method to calculate the guaranteed equity based on insurance after a fixed contract period for each individual.
* includes modest estimate for 10 year period on example salaries.

Accurate calculation are based on the user-input banking statements

The example dataset is completely fictional but the income and expenses are within the accurate range for 2 persons with shared rent/mortgage on a modest income in Switzerland, aged ~30. 

The dataset includes normal ranges for daily living expenses, random large payments like a car, or home repairs, or moving homes with a different rental price. 

The dataset simulates the saving requirements to purchase a home 100% within 10 years, or a mortgage within 3 years (incl 20% deposit) with full purchase in 10.

Investments or other sources of interest are not simulated. Healthy investment returns on liquid cash should increase the rates shown here. Untaxed saving like 3rd pillar are also not included.

For an example mortgage, Swiss lenders provide as low as 1% fixed interest rates, with amortizations of ~400-600 per month for up to 1'000'000 CHF. With this model you could potentially negotiate less than 1200 per month in interest + amortization and pay off this loan within 10 years as illustrated. This model also includes "safety buffers" allowing for ~1-2years of insured/uninsured unemployment to simulate the worst-case scenario without risk of failure to repay.

For home buyers, further expenses can also be included to explicitly illustrate the costs allocated for maintenance, repairs, tax, and emergency funds. 
Example summary with fictional data:

 [home_buyer](home_buyer/img/equity_combined.pdf)
