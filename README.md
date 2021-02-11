# CEDS
The Community Emissions Data System (CEDS) produces consistent estimates of global air emissions species over the industrial era (1750 - present). The system is written in R and uses open-source data (with the exception of the IEA energy statistics which must be purchased from IEA). CEDS is publicly available through an [Open Source License](#license-section).


**Feb 2021 Release:** February 05, 2021 (v\_2021\_02\_05)

This data and code release extends the emissions time series to 2019 and updates driver and emissions data throughout. This version builds on the extension of the CEDS system to 2017 described in [McDuffie et al. 2020](https://essd.copernicus.org/preprints/essd-2020-103/). Major features:

* Emissions estimates to 2019
* Updated default data from GAINS and EDGAR
* Updates to country inventories used for scaling
* Improved BC/OC emission time series
* Improved consistency over time
* Updated code and driver data

For details on this release see:

* See the [release notes](https://github.com/JGCRI/CEDS/wiki/Release-Notes) for a summary of changes.
* [Graphs of emission differences](./documentation/Version_comparison_figures_v_2021_02_05_vs_v_2016_07_16(CMIP6).pdf) between this version and the CEDS CMIP6 data release documented in Hoesly et al (2018a). 
* [Graphs of emission differences](./documentation/Version_comparison_figures_v_2021_02_05_vs_v_2019_12_23.pdf) between this version and the previous December 2019 CEDS data release. 
* Emissions by country and sector, archived [here](http://doi.org/10.5281/zenodo.4509372).

We encourage comments on this data. The best way to comment on the data is through the [CEDS Issues](https://github.com/JGCRI/CEDS/issues) page.

_Gridded data corresponding to this release is in production and will be released shortly._

_A journal paper describing this dataset is in preparation. A notice will be updated here and sent to the CEDS listserv when this is available._

***

Documentation of CEDS assumptions and system operation, including a user guide, are available at the [CEDS project wiki](https://github.com/JGCRI/CEDS/wiki) and in the journal papers noted below. 

Current issues with the data or system are documented in the [CEDS Issues](https://github.com/JGCRI/CEDS/issues) system in this GitHub repository. Users can submit issues using this system. These can include anomalies found in either the aggregate or gridded emissions data. Please use an appropriate tag for any submitted issues. Note that by default only unresolved issues are shown. All issues, including resolved issues, can be viewed by removing the "is:open" filter. *Issues relevant for CMIP6 data releases are tagged with a “CMIP6” label (note that issues will be closed when resolved in subsequent CEDS data releases, but are still available for viewing.)*

Further information can also be found at the [project web site](http://www.globalchange.umd.edu/ceds/), including a [CMIP6 page](http://www.globalchange.umd.edu/ceds/ceds-cmip6-data/) that provides details for obtaining gridded emission datasets produced by this project for use in CMIP6. You can also sign up for data release announcements from the CEDSinfo listserv following the instructions on the [project web site](http://www.globalchange.umd.edu/ceds/).

If you plan to use the CEDS data system for a research project you are encouraged to contact [Steve Smith](mailto:ssmith@pnnl.gov) so that we can coordinate with any on-going work on the CEDS system and make sure we are not duplicating effort. CEDS is research software, and we will be happy to help and make sure that you are able to make the best possible use of this system.

Users should use the most recent version of this repository, which will include maintenance updates to address documentation or usability issues. Major changes that alter emission estimates or system structure and use will be noted in the [release notes](https://github.com/JGCRI/CEDS/wiki/Release-Notes).

CEDS has only been possible through the participation of many collaborators. Our **collaboration policy** is that collaborators who contribute unpublished data used in CEDS updates will be included as co-authors on the journal paper that describes the next CEDS major release. We particularly encourage contributions of updated emission information from countries or sectors not well represented in the data currently used in CEDS.

# Data Reference

Reference for [this data version](https://github.com/JGCRI/CEDS/wiki/Release-Notes):
O'Rourke, P. R, Smith, S. J., Mott, A., Ahsan, H., McDuffie, E. E., Crippa, M., Klimont, S., McDonald, B., Z., Wang, Nicholson, M. B, Feng, L., and Hoesly, R. M. (2021, February 05). CEDS v-2021-02-05 Emission Data 1975-2019 (Version Feb-05-2021). [Zenodo. http://doi.org/10.5281/zenodo.4509372](http://doi.org/10.5281/zenodo.4509372).

# Journal Papers
[Hoesly et al, Historical (1750–2014) anthropogenic emissions of reactive gases and aerosols from the Community Emissions Data System (CEDS). ](https://www.geosci-model-dev.net/11/369/2018/gmd-11-369-2018.html) _Geosci. Model Dev._ 11, 369-408, 2018a.

_Note that the paper zip file supplement contains annual emissions estimates by country and sector for the July 26, 2016 data version. The most recent data is available from the links above._

[Hoesly et al Informing energy consumption uncertainty: an analysis of energy data revisions.”](https://iopscience.iop.org/article/10.1088/1748-9326/aaebc3/meta) _Environ. Res. Lett._ 13 124023, 2018b.

[Feng et al, The generation of gridded emissions data for CMIP6.](https://gmd.copernicus.org/articles/13/461/2020/) _Geosci. Model Dev._ 13, 461–482, 2020.

# <a name="license-section"></a>License
Copyright © 2017, Battelle Memorial Institute
All rights reserved.

1.	Battelle Memorial Institute (hereinafter Battelle) hereby grants permission to any person or entity lawfully obtaining a copy of this software and associated documentation files (hereinafter “the Software”) to redistribute and use the Software in source and binary forms, with or without modification.  Such person or entity may use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and may permit others to do so, subject to the following conditions:

    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimers. 
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
    * Other than as used herein, neither the name Battelle Memorial Institute or Battelle may be used in any form whatsoever without the express written consent of Battelle.

2.	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL BATTELLE OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
