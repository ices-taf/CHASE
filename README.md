CHASE
================

## HELCOM Hazardous Substances Status Assessment Tool (CHASE)

The CHASE tool integrates data on hazardous substances in water,
sediments and biota as well as bio-effect indicators and is based on a
substance- or bio-effect-specific calculation of a ‘contamination
ratio’, being the ratio between an observed concentration and a
threshold value. Values \<1.0 indicate areas ‘unaffected’, while values
\>1.0 indicate areas potentially ‘affected’. These ratios are combined
within the matrices, water, sediment and biota and for biological
effects and the overall assessment for each assessment unit is
determined by use of a ‘one out, all out principle’ with regard to each
matrix. That is, the ‘worst’ matrix determines the status for the
assessment unit in question.

In addition to this primary status assessment, the CHASE tool makes a
secondary assessment, estimating the confidence if the primary
assessment result. This is done by combining a number of qualitative and
semi-quantitative confidence parameters for each indicator.

#### Assessment function

The CHASE assessment is carried out by calling the function
`CHASEassessment`. The source code for this function is contained in
[CHASE.R](src/CHASE.R) and auxiliary functions are contained in
[CHASE_functions.R](src/CHASE_functions.R). These two files contain the
code required for making the assessment. Other files in this repository
are used only for testing.

``` r
  source('src/CHASE.R')
  result <- CHASEassessment(indicator_dataframe)
```

#### Input data

A required argument for the function is a dataframe containing the
assessment indicator data. Required column names are seen in the table
here:

| Column        | Required        | Description                                                    |
|:--------------|:----------------|:---------------------------------------------------------------|
| *AU*          | NO<sup>1</sup>  | assessment unit name or identifier                             |
| *Matrix*      | YES             | Water, Sediment, Biota *or* Biological effects                 |
| *Substance*   | YES             | name of indicator                                              |
| *Type*        | YES             | HM / Org                                                       |
| *Threshold*   | NO              | concentration limit                                            |
| *Status*      | NO              | observed concentration                                         |
| *CR*          | YES<sup>2</sup> | ratio of status to threshold                                   |
| *ConfTemp*    | NO<sup>3</sup>  | temporal confidence: H\[igh\], M\[oderate\] *or* L\[ow\]       |
| *ConfSpatial* | NO<sup>3</sup>  | temporal confidence: H\[igh\], M\[oderate\] *or* L\[ow\]       |
| *ConfAcc*     | NO<sup>3</sup>  | accuracy confidence: H\[igh\], M\[oderate\] *or* L\[ow\]       |
| *ConfMethod*  | NO<sup>3</sup>  | methodological confidence: H\[igh\], M\[oderate\] *or* L\[ow\] |
| *ConfThresh*  | NO<sup>3</sup>  | threshold confidence: H\[igh\], M\[oderate\] *or* L\[ow\]      |

<sup>1</sup>If no assessment units are provided, the indicators will be
aggregated for a single assessment.  
<sup>2</sup>If *CR* is not provided, then *Threshold* and *Status* are
required columns.  
<sup>3</sup>Confidence components are not required in order to give a
status assessment. If any single confidence component is not provided,
it will be assumed to be *Low*.

#### Results

By default, the function returns a list of 4 dataframes:

1.  **‘Indicators’** is essentially the same as the input dataframe,
    with the addition of overall confidence score for each indicator, as
    well as results for the AU/matrix combination to which the indicator
    belongs.
2.  **‘Matrix by row’** gives an aggregated result with each combination
    of AU/matrix on a separate row.
3.  **‘Matrix by column’** gives an aggregated result with each AU on a
    separate row and Matrices in columns.
4.  **‘AssessmentUnits’** gives an aggregated overall result for each
    AU.

#### Test

The CHASE assessment is tested here: [CHASE_test.md](CHASE_test.md)
