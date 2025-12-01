# Walkthrough (population-level)

## Overview

## Walkthrough

This workflow is in Silva *et al.* (in prep), and is a simplified case
study, designed for quick execution. We will use a GPS tracking dataset
of African buffalos (*Syncerus caffer*) tracked in Kruger National Park
between 2005 and 2006 ([Cross et al., 2009](#ref-cross2009)), to inform
our simulations. Our primary goal is to reliably estimate **mean home
range area** of a population of African Buffalos. Please see the
manuscript for a more detailed workflow, incorporating both research
targets.

![](images/tutorial2_img1.png) Similarly to the [single
estimate](https://ecoisilva.github.io/movedesign/articles/movedesign.html)
vignette, we begin by selecting the appropriate workflow in the
![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNTc2IDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MS4xMmVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDpjdXJyZW50Q29sb3I7b3ZlcmZsb3c6dmlzaWJsZTtwb3NpdGlvbjpyZWxhdGl2ZTsiPjxwYXRoIGQ9Ik01NzUuOCAyNTUuNWMwIDE4LTE1IDMyLjEtMzIgMzIuMWgtMzJsLjcgMTYwLjJjMCAyLjctLjIgNS40LS41IDguMVY0NzJjMCAyMi4xLTE3LjkgNDAtNDAgNDBINDU2Yy0xLjEgMC0yLjIgMC0zLjMtLjFjLTEuNCAuMS0yLjggLjEtNC4yIC4xSDQxNiAzOTJjLTIyLjEgMC00MC0xNy45LTQwLTQwVjQ0OCAzODRjMC0xNy43LTE0LjMtMzItMzItMzJIMjU2Yy0xNy43IDAtMzIgMTQuMy0zMiAzMnY2NCAyNGMwIDIyLjEtMTcuOSA0MC00MCA0MEgxNjAgMTI4LjFjLTEuNSAwLTMtLjEtNC41LS4yYy0xLjIgLjEtMi40IC4yLTMuNiAuMkgxMDRjLTIyLjEgMC00MC0xNy45LTQwLTQwVjM2MGMwLS45IDAtMS45IC4xLTIuOFYyODcuNkgzMmMtMTggMC0zMi0xNC0zMi0zMi4xYzAtOSAzLTE3IDEwLTI0TDI2Ni40IDhjNy03IDE1LTggMjItOHMxNSAyIDIxIDdMNTY0LjggMjMxLjVjOCA3IDEyIDE1IDExIDI0eiIgLz48L3N2Zz4=)`Home`
tab. Please check that vignette first, as some information is omitted
here to avoid repetition. In this tutorial, we will showcase a workflow
for **deploying a set number of units**.

![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNDQ4IDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MC44OGVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDpjdXJyZW50Q29sb3I7b3ZlcmZsb3c6dmlzaWJsZTtwb3NpdGlvbjpyZWxhdGl2ZTsiPjxwYXRoIGQ9Ik0yMjQgMGMtMTcuNyAwLTMyIDE0LjMtMzIgMzJWNTEuMkMxMTkgNjYgNjQgMTMwLjYgNjQgMjA4djI1LjRjMCA0NS40LTE1LjUgODkuNS00My44IDEyNC45TDUuMyAzNzdjLTUuOCA3LjItNi45IDE3LjEtMi45IDI1LjRTMTQuOCA0MTYgMjQgNDE2SDQyNGM5LjIgMCAxNy42LTUuMyAyMS42LTEzLjZzMi45LTE4LjItMi45LTI1LjRsLTE0LjktMTguNkMzOTkuNSAzMjIuOSAzODQgMjc4LjggMzg0IDIzMy40VjIwOGMwLTc3LjQtNTUtMTQyLTEyOC0xNTYuOFYzMmMwLTE3LjctMTQuMy0zMi0zMi0zMnptMCA5NmM2MS45IDAgMTEyIDUwLjEgMTEyIDExMnYyNS40YzAgNDcuOSAxMy45IDk0LjYgMzkuNyAxMzQuNkg3Mi4zQzk4LjEgMzI4IDExMiAyODEuMyAxMTIgMjMzLjRWMjA4YzAtNjEuOSA1MC4xLTExMiAxMTItMTEyem02NCAzNTJIMjI0IDE2MGMwIDE3IDYuNyAzMy4zIDE4LjcgNDUuM3MyOC4zIDE4LjcgNDUuMyAxOC43czMzLjMtNi43IDQ1LjMtMTguN3MxOC43LTI4LjMgMTguNy00NS4zeiIgLz48L3N2Zz4=)
Please choose `Select` as your data source to choose from a list of
available species. Then, set `Home range` as your research target. For
the analytical target, choose `Mean estimate of sampled population`. A
new option will show up for deployment; please select the
`"I plan to deploy a set number of VHF/GPS tags."` In addition, tick the
`Add individual variation` checkbox. This allows us to account for
individual differences, rather than assuming all individuals behave
identically.

#### Data

Differently from the [single
estimate](https://ecoisilva.github.io/movedesign/articles/movedesign.html)
workflow, we are now able to select multiple individuals, which enable
us to extract **population-level species parameters**.

First, select the `African Buffalo` (*Syncerus caffer*) as your study
species. From the dropdown menu, select all six individuals and click
the `'Validate'` button. Once validation is successful, the button
should now read `'Validated!'`.

![](images/tutorial2_img2.png)

Before proceeding further, you should visually inspect the variograms to
confirm range-residency through the
![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNTEyIDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MWVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDpjdXJyZW50Q29sb3I7b3ZlcmZsb3c6dmlzaWJsZTtwb3NpdGlvbjpyZWxhdGl2ZTsiPjxwYXRoIGQ9Ik02NCA2NGMwLTE3LjctMTQuMy0zMi0zMi0zMlMwIDQ2LjMgMCA2NFY0MDBjMCA0NC4yIDM1LjggODAgODAgODBINDgwYzE3LjcgMCAzMi0xNC4zIDMyLTMycy0xNC4zLTMyLTMyLTMySDgwYy04LjggMC0xNi03LjItMTYtMTZWNjR6bTQwNi42IDg2LjZjMTIuNS0xMi41IDEyLjUtMzIuOCAwLTQ1LjNzLTMyLjgtMTIuNS00NS4zIDBMMzIwIDIxMC43bC01Ny40LTU3LjRjLTEyLjUtMTIuNS0zMi44LTEyLjUtNDUuMyAwbC0xMTIgMTEyYy0xMi41IDEyLjUtMTIuNSAzMi44IDAgNDUuM3MzMi44IDEyLjUgNDUuMyAwTDI0MCAyMjEuM2w1Ny40IDU3LjRjMTIuNSAxMi41IDMyLjggMTIuNSA0NS4zIDBsMTI4LTEyOHoiIC8+PC9zdmc+)`Variogram`
tab of the `Data Visualization` box. Variograms allow users to check if
semivariance reaches an asymptote, and facilitate a cursory confirmation
of range residency ([Calabrese et al., 2016](#ref-calabrese2016); [Silva
et al., 2022](#ref-silva2022)). Downstream results may be unreliable if
we violate the range residency assumption as, from this point onward,
the `movedesign` application will operate under the assumption that the
data originates from a range-resident species.

![](images/tutorial2_img3.png)

For this tutorial, we will proceed with all six individuals. In a real
workflow, however, we would likely exclude `Gabs` and proceed with the
remaining individuals.

Proceed by clicking the `'Extract'` button. Upon successful extraction,
the `Displaying parameters` box presents our extracted species
parameters: the **position autocorrelation** ($\tau_{p}$) and the
**velocity autocorrelation timescale** ($\tau_{v}$). For the African
buffalos, the mean $\tau_{p}$ is 10.1 days (95% CI: 6.9, 14.7), and a
mean $\tau_{v}$ is 32.5 minutes (95% CI: 24.9, 42.6). These parameters
serve as the foundation for all subsequent simulations as we evaluate
study design.

#### Sampling design

Next, we navigate to the
![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNDQ4IDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MC44OGVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDojMDA5REEwO292ZXJmbG93OnZpc2libGU7cG9zaXRpb246cmVsYXRpdmU7Ij48cGF0aCBkPSJNMTc2IDBjLTE3LjcgMC0zMiAxNC4zLTMyIDMyczE0LjMgMzIgMzIgMzJoMTZWOTguNEM5Mi4zIDExMy44IDE2IDIwMCAxNiAzMDRjMCAxMTQuOSA5My4xIDIwOCAyMDggMjA4czIwOC05My4xIDIwOC0yMDhjMC00MS44LTEyLjMtODAuNy0zMy41LTExMy4ybDI0LjEtMjQuMWMxMi41LTEyLjUgMTIuNS0zMi44IDAtNDUuM3MtMzIuOC0xMi41LTQ1LjMgMEwzNTUuNyAxNDNjLTI4LjEtMjMtNjIuMi0zOC44LTk5LjctNDQuNlY2NGgxNmMxNy43IDAgMzItMTQuMyAzMi0zMnMtMTQuMy0zMi0zMi0zMkgyMjQgMTc2em03MiAxOTJWMzIwYzAgMTMuMy0xMC43IDI0LTI0IDI0cy0yNC0xMC43LTI0LTI0VjE5MmMwLTEzLjMgMTAuNy0yNCAyNC0yNHMyNCAxMC43IDI0IDI0eiIgLz48L3N2Zz4=)`'Sampling design'`
tab, where we input our sampling parameters. For this tutorial, we will
consider the following sampling schedule: a ***sampling duration*** of
`3 months`, with 12 new locations collected per day (***sampling
interval*** of `2 hours`). To set this particular schedule, we ensure
that `GPS/Satellite logger` is selected in the first dropdown menu, and
untick the `Select from plot` checkbox in the `Device settings` box
before proceeding, which will prompt us to manually input the sampling
interval. Then, we set `GPS battery life` (equivalent to the maximum
sampling duration) to `3 months` and
`What sampling interval will you evaluate?` to `2 hours`.

We incorporate three additional components: **fix success rate** (here
expected to be, on average, `85%`), reflecting the reliability of the
GPS signal; **tag failure** (`5%` chance of a tag failing during data
collection); and **location error** (averaging `15 meters`). Please
Enable all three settings. At this point, we click the `Validate` button
(verifying once again that it switches to `Validated!`) and, afterwards,
the `Run` button.

![](images/tutorial2_img4.png)

Once a message appears confirming that this step has been successfully
completed, we proceed to the
![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNTc2IDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MS4xMmVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDpjdXJyZW50Q29sb3I7b3ZlcmZsb3c6dmlzaWJsZTtwb3NpdGlvbjpyZWxhdGl2ZTsiPjxwYXRoIGQ9Ik00MDggMTIwYzAgNTQuNi03My4xIDE1MS45LTEwNS4yIDE5MmMtNy43IDkuNi0yMiA5LjYtMjkuNiAwQzI0MS4xIDI3MS45IDE2OCAxNzQuNiAxNjggMTIwQzE2OCA1My43IDIyMS43IDAgMjg4IDBzMTIwIDUzLjcgMTIwIDEyMHptOCA4MC40YzMuNS02LjkgNi43LTEzLjggOS42LTIwLjZjLjUtMS4yIDEtMi41IDEuNS0zLjdsMTE2LTQ2LjRDNTU4LjkgMTIzLjQgNTc2IDEzNSA1NzYgMTUyVjQyMi44YzAgOS44LTYgMTguNi0xNS4xIDIyLjNMNDE2IDUwM1YyMDAuNHpNMTM3LjYgMTM4LjNjMi40IDE0LjEgNy4yIDI4LjMgMTIuOCA0MS41YzIuOSA2LjggNi4xIDEzLjcgOS42IDIwLjZWNDUxLjhMMzIuOSA1MDIuN0MxNy4xIDUwOSAwIDQ5Ny40IDAgNDgwLjRWMjA5LjZjMC05LjggNi0xOC42IDE1LjEtMjIuM2wxMjIuNi00OXpNMzI3LjggMzMyYzEzLjktMTcuNCAzNS43LTQ1LjcgNTYuMi03N1Y1MDQuM0wxOTIgNDQ5LjRWMjU1YzIwLjUgMzEuMyA0Mi4zIDU5LjYgNTYuMiA3N2MyMC41IDI1LjYgNTkuMSAyNS42IDc5LjYgMHpNMjg4IDE1MmE0MCA0MCAwIDEgMCAwLTgwIDQwIDQwIDAgMSAwIDAgODB6IiAvPjwvc3ZnPg==)`Home range`
tab below
![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNTEyIDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MWVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDpjdXJyZW50Q29sb3I7b3ZlcmZsb3c6dmlzaWJsZTtwb3NpdGlvbjpyZWxhdGl2ZTsiPjxwYXRoIGQ9Ik0zNTIgOTZjMCAxNC4zLTMuMSAyNy45LTguOCA0MC4yTDM5NiAyMjcuNGMtMjMuNyAyNS4zLTU0LjIgNDQuMS04OC41IDUzLjZMMjU2IDE5MmgwIDBsLTY4IDExNy41YzIxLjUgNi44IDQ0LjMgMTAuNSA2OC4xIDEwLjVjNzAuNyAwIDEzMy44LTMyLjcgMTc0LjktODRjMTEuMS0xMy44IDMxLjItMTYgNDUtNXMxNiAzMS4yIDUgNDVDNDI4LjEgMzQxLjggMzQ3IDM4NCAyNTYgMzg0Yy0zNS40IDAtNjkuNC02LjQtMTAwLjctMTguMUw5OC43IDQ2My43Qzk0IDQ3MS44IDg3IDQ3OC40IDc4LjYgNDgyLjZMMjMuMiA1MTAuM2MtNSAyLjUtMTAuOSAyLjItMTUuNi0uN1MwIDUwMS41IDAgNDk2VjQ0MC42YzAtOC40IDIuMi0xNi43IDYuNS0yNC4xbDYwLTEwMy43QzUzLjcgMzAxLjYgNDEuOCAyODkuMyAzMS4yIDI3NmMtMTEuMS0xMy44LTguOC0zMy45IDUtNDVzMzMuOS04LjggNDUgNWM1LjcgNy4xIDExLjggMTMuOCAxOC4yIDIwLjFsNjkuNC0xMTkuOWMtNS42LTEyLjItOC44LTI1LjgtOC44LTQwLjJjMC01MyA0My05NiA5Ni05NnM5NiA0MyA5NiA5NnptMjEgMjk3LjljMzIuNi0xMi44IDYyLjUtMzAuOCA4OC45LTUyLjlsNDMuNyA3NS41YzQuMiA3LjMgNi41IDE1LjYgNi41IDI0LjFWNDk2YzAgNS41LTIuOSAxMC43LTcuNiAxMy42cy0xMC42IDMuMi0xNS42IC43bC01NS40LTI3LjdjLTguNC00LjItMTUuNC0xMC44LTIwLjEtMTguOUwzNzMgMzkzLjl6TTI1NiAxMjhhMzIgMzIgMCAxIDAgMC02NCAzMiAzMiAwIDEgMCAwIDY0eiIgLz48L3N2Zz4=)`Analyses`.

#### Analyses

##### Home range estimation

To start the estimation process, we click the `Run estimation` button
located in the top box. We can now see the outputs for a single
simulation, providing a starting point. The relative error in home range
area is an overestimation of `4.5%` (95% CI: `-56.9`, `344%`). Note that
due to the randomized nature of seed generation and the inclusion of
individual variation, values may differ substantially across runs.

![](images/tutorial2_img5.png) We can view the outputs for each
simulation using the `Show simulation no.:` slider. Most importantly, in
the `Simulations` box (top right corner), we set the total number of
tags and the error threshold. We set the error threshold for our
estimates at `±5%`, and the total number of tags to `20` individuals,
before clicking the `Simulate` button. A message then appears,
indicating the expected runtime; we must wait for this process to
complete before we can explore all the outputs.

#### Meta-analyses

Next, in the `Meta-analyses` tab, we click the `Run meta-analyses`
button to obtain information on population-level inferences. Once
completed, a new box on the left indicates that, on average, the
population-level home range area is underestimated by `-11%` (`-29.2`,
`9.6%`). In addition, two plots are generated: first with the
**individual estimates of home range areas**, and then the
**population-level estimates of home range areas**.

![](images/tutorial2_img6.png)

The first plot illustrates the relative error in **individual-level home
range area estimates** by displaying the estimated home range area
(x-axis, in km²) for each individual (y-axis), along with the associated
95% confidence intervals. The black square represents the
population-level estimate (mean across all individuals) with its
corresponding 95% confidence interval. The vertical solid line indicate
the expected true value for the inputted species parameters.

The second plot illustrates how an increasing number of individuals
(from a **population sample size** of `2` to a maximum of `20`) affects
our population-level mean estimates. Each point represents the mean
relative error (%) of this metric and its associated 95% confidence
intervals, plotted against the number of tracked individuals. The dashed
horizontal lines indicate a predefined error threshold of ±5%. An
accompanying table provides detailed numerical values, as well as
whether a sub-population was detected at each population sample size.

To verify these outputs, we can resample through **combination testing**
to assess the spread of estimates, randomly reassigning individuals into
new sets and rerunning the estimation of population-level mean estimates
multiple times.

![](images/tutorial2_img7.png)

This step helps evaluate how individual estimates contribute to the
variation of the observed mean estimate. We set the number of resamples
to `15` here, though higher values are recommended for larger population
samples. After clicking the `Resample` button, the new plot illustrates
how different sets of individuals shape the observed mean estimate
across increasing population sample sizes. Variation remains high,
indicating that additional simulations may be needed to stabilize the
mean home range estimate.

#### Report

Finally, in the
![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNTEyIDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MWVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDpjdXJyZW50Q29sb3I7b3ZlcmZsb3c6dmlzaWJsZTtwb3NpdGlvbjpyZWxhdGl2ZTsiPjxwYXRoIGQ9Ik0zMiAzMkg0ODBjMTcuNyAwIDMyIDE0LjMgMzIgMzJWOTZjMCAxNy43LTE0LjMgMzItMzIgMzJIMzJDMTQuMyAxMjggMCAxMTMuNyAwIDk2VjY0QzAgNDYuMyAxNC4zIDMyIDMyIDMyem0wIDEyOEg0ODBWNDE2YzAgMzUuMy0yOC43IDY0LTY0IDY0SDk2Yy0zNS4zIDAtNjQtMjguNy02NC02NFYxNjB6bTEyOCA4MGMwIDguOCA3LjIgMTYgMTYgMTZIMzM2YzguOCAwIDE2LTcuMiAxNi0xNnMtNy4yLTE2LTE2LTE2SDE3NmMtOC44IDAtMTYgNy4yLTE2IDE2eiIgLz48L3N2Zz4=)`Report`
tab, clicking `Build report` generates a comprehensive summary of our
sampling schedule outputs. This report consolidates key findings,
highlighting how our current sampling effort affects estimation
accuracy. Specifically, it reveals that a **population sample size** of
`20` individuals fails to meet the `±5%` error threshold for **mean home
range area**, suggesting insufficient data to achieve reliable
estimates. Although the confidence intervals overlap with the error
margins, the observed individual variation may still lead to inaccurate
conclusions. The report provides a clear visual representation of these
uncertainties, allowing us to assess whether adjustments to the sampling
schedule are necessary.

![](images/tutorial2_img8.png)

### References

Calabrese, J. M., Fleming, C. H., & Gurarie, E. (2016). Ctmm: An r
package for analyzing animal relocation data as a continuous-time
stochastic process. *Methods in Ecology and Evolution*, *7*(9),
1124–1132.

Cross, P. C., Heisey, D. M., Bowers, J. A., Hay, C. T., Wolhuter, J.,
Buss, P., Hofmeyr, M., Michel, A. L., Bengis, R. G., & Bird, T. L. F.
(2009). Disease, predation and demography: Assessing the impacts of
bovine tuberculosis on african buffalo by monitoring at individual and
population levels. *Journal of Applied Ecology*, *46*(2), 467–475.

Silva, I., Fleming, C. H., Noonan, M. J., Alston, J., Folta, C., Fagan,
W. F., & Calabrese, J. M. (2022). Autocorrelation-informed home range
estimation: A review and practical guide. *Methods in Ecology and
Evolution*, *13*(3), 534–544. <https://doi.org/10.1111/2041-210X.13786>
