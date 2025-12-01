# movedesign

## Why `movedesign`?

`movedesign` is built using R language with Shiny for an easy-to-use
user interface (GUI). This application will allow you to test different
tracking schedules while considering an initially set research question
(currently *home range* and *speed/distance* estimation).

- Doesn’t require R coding experience.
- Leverages the `ctmm` R package for statistically unbiased methods.

## How to start:

The application includes a built-in guided tutorial to help you navigate
its features. When you open the `'Home'` tab, you’ll find the following:

![](images/tutorial1_img1.png)

This step-by-step guide will walk you through the app, ensuring you
understand its features and functionality. When an action is required,
it will be clearly highlighted. Please follow these instructions
carefully, as each step builds on the previous one. If no action is
needed, simply continue to the next step by clicking `'Next'` or
pressing the right arrow key on your keyboard. The information from the
guided tour is also partially available below.

![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNTEyIDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MWVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDojREI0NTQ1O292ZXJmbG93OnZpc2libGU7cG9zaXRpb246cmVsYXRpdmU7Ij48cGF0aCBkPSJNMjU2IDUxMkEyNTYgMjU2IDAgMSAwIDI1NiAwYTI1NiAyNTYgMCAxIDAgMCA1MTJ6bTAtMzg0YzEzLjMgMCAyNCAxMC43IDI0IDI0VjI2NGMwIDEzLjMtMTAuNyAyNC0yNCAyNHMtMjQtMTAuNy0yNC0yNFYxNTJjMC0xMy4zIDEwLjctMjQgMjQtMjR6TTIyNCAzNTJhMzIgMzIgMCAxIDEgNjQgMCAzMiAzMiAwIDEgMSAtNjQgMHoiIC8+PC9zdmc+)Warning:
During the guided tutorial, refrain from interacting with anything
outside the highlighted zones to avoid interruptions.

This tutorial provides an overview of key features but does not cover
detailed definitions. For more in-depth explanations, you can access
comprehensive
![](data:image/svg+xml;base64,PHN2ZyBhcmlhLWhpZGRlbj0idHJ1ZSIgcm9sZT0iaW1nIiB2aWV3Ym94PSIwIDAgNTEyIDUxMiIgc3R5bGU9ImhlaWdodDoxZW07d2lkdGg6MWVtO3ZlcnRpY2FsLWFsaWduOi0wLjEyNWVtO21hcmdpbi1sZWZ0OmF1dG87bWFyZ2luLXJpZ2h0OmF1dG87Zm9udC1zaXplOmluaGVyaXQ7ZmlsbDpjdXJyZW50Q29sb3I7b3ZlcmZsb3c6dmlzaWJsZTtwb3NpdGlvbjpyZWxhdGl2ZTsiPjxwYXRoIGQ9Ik00NjQgMjU2QTIwOCAyMDggMCAxIDAgNDggMjU2YTIwOCAyMDggMCAxIDAgNDE2IDB6TTAgMjU2YTI1NiAyNTYgMCAxIDEgNTEyIDBBMjU2IDI1NiAwIDEgMSAwIDI1NnptMTY5LjgtOTAuN2M3LjktMjIuMyAyOS4xLTM3LjMgNTIuOC0zNy4zaDU4LjNjMzQuOSAwIDYzLjEgMjguMyA2My4xIDYzLjFjMCAyMi42LTEyLjEgNDMuNS0zMS43IDU0LjhMMjgwIDI2NC40Yy0uMiAxMy0xMC45IDIzLjYtMjQgMjMuNmMtMTMuMyAwLTI0LTEwLjctMjQtMjRWMjUwLjVjMC04LjYgNC42LTE2LjUgMTIuMS0yMC44bDQ0LjMtMjUuNGM0LjctMi43IDcuNi03LjcgNy42LTEzLjFjMC04LjQtNi44LTE1LjEtMTUuMS0xNS4xSDIyMi42Yy0zLjQgMC02LjQgMi4xLTcuNSA1LjNsLS40IDEuMmMtNC40IDEyLjUtMTguMiAxOS0zMC42IDE0LjZzLTE5LTE4LjItMTQuNi0zMC42bC40LTEuMnpNMjI0IDM1MmEzMiAzMiAwIDEgMSA2NCAwIDMyIDMyIDAgMSAxIC02NCAweiIgLz48L3N2Zz4=)**help
tips** at any time. Documentation is also available on Silva *et al.*
([2023](#ref-silva2023movedesign)).

### Workflows

These are the current options for `movedesign` workflows. Users may
configure their study design by selecting different options for data
source, research target, and analytical target. All workflows follow a
stepwise approach, with tabs displayed sequentially on the right
sidebar. Irrelevant tabs for the current workflow will be automatically
hidden.

**Step 1.** *Data source:*

Users can specify what is their data source:

`Upload`: Import a dataset from a local file.

`Select`: Choose from pre-existing datasets available in the
application.

`Simulate`: Generate synthetic data for testing or modeling purposes.

**Step 2.** *Research target:*

Users can define their research targets:

`Home range`: Estimate long-term space-use requirements.

`Speed & distance`: Estimate fine-scale movement metrics, such as speed
and distance traveled.

**Step 3.** *Analytical target:*

Users can decide on their analytical target for the estimates:

`Individual estimate`: Obtain metrics for a single individual.

`Mean estimate of sampled population`: Obtain a mean estimate across a
sampled group.

`Compare estimates of two sampled groups`: Perform a comparative
analysis between two groups within a tracked population (for the
detection of sub-groups).

Additionally, users have the option to:

`Add individual variation`: Include variation among individuals during
simulations (only available for mean and estimate comparisons).

Go
[here](https://ecoisilva.github.io/movedesign/articles/tutorial_ind.html)
for more detailed information regarding the `Individual estimate`
workflow.

### Related work

- The [ctmm](https://github.com/ctmm-initiative/ctmm) R package and
  [ctmmweb](https://github.com/ctmm-initiative/ctmmweb) Shiny
  application.

### References

Silva, I., Fleming, C. H., Noonan, M. J., Fagan, W. F., & Calabrese, J.
M. (2023). Movedesign: Shiny r app to evaluate sampling design for
animal movement studies. *Methods in Ecology and Evolution*, *14*(9),
2216–2225. <https://doi.org/10.1111/2041-210X.14153>
