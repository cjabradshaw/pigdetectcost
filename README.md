# Cost of detecting pigs
Relative costs of different detection models for low feral pig densities on Kangaroo Island
<img align="right" src="www/pigtarget.png" width="300" style="margin-top: 20px"></a>

<a href="https://www.flinders.edu.au/people/corey.bradshaw">Corey Bradshaw</a><br>
Global Ecology<br>
Flinders University<br>
May 2024<br>

### Approach
We derived cost data from the <a href="https://pir.sa.gov.au/">South Australia Department of Primary Industries and Regions</a> (PIRSA) for five methods used to detect dogs: (<em>i</em>) teams of two dogs (with handler), (<em>ii</em>) thermal-assisted helicopter surveys, (<em>iii</em>) camera traps (fixed), (<em>iv</em>) environmental DNA (eDNA) kits, and (<em>v</em>) ground surveys by PIRSA staff. To determine the relative costs per method with an incrementing number of remaining pigs (1 to 100), we first assumed that approximately one-third of Kangaroo Island would have to be surveyed for remaining pigs (approximately 1468 km<sup>2</sup>). We then sourced pig movement and home range data from <a href="http://doi.org/10.1186/s40462-017-0105-1">Kay et al. (2017)</a>, calculating the average monthly home-range size of 3.4 km<sup>2</sup>, and assuming a uniform distribution of remaining pigs on the island. For each method, we calculated the probability of detection based on its cumulative spatial coverage over one month, adjusting the emergent detection probabilities and costs for an increasing number of pigs from 1 to 100. For camera traps and eDNA kits, detection probability is a function of the number of cameras or kits purchased, respectively, as well as the assumption of uniform placement in the landscape. We also removed the cost of 500 cameras (AU$275,000) across all scenarios given these items have already been purchased and do not factor into projected budgets. All costs are expressed in 2024 AU$ and should be interpreted as relative rather than absolute given the assumptions.

### Scripts
- <code>pigdetectcost.R</code>

### Data
- cost & pig-movement data provided in the R script file

<br>
<a href="https://www.flinders.edu.au"><img align="bottom-left" src="www/Flinders_University_Logo_Horizontal_RGB_Master.png" alt="Flinders University logo" height="40" style="margin-top: 20px"></a> &nbsp; <a href="https://globalecologyflinders.com"><img align="bottom-left" src="www/GEL Logo Kaurna New Transp-2.png" alt="GEL logo" height="85" style="margin-top: 20px"></a> &nbsp; <a href="https://pir.sa.gov.au"><img align="bottom-left" src="www/PIRSAlogo.svg" alt="PIRSA logo" height="70" style="margin-top: 20px"></a>
