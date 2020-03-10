# CEDS
The Community Emissions Data System (CEDS) produces consistent estimates of global air emissions species over the industrial era (1750 - present). The system is written in R and uses open-source data (with the exception of the IEA energy statistics which must be purchased from IEA). CEDS is publicly available through an [Open Source License](#license-section).

***
**Public Release:** December 23, 2019. The current code and data in the repository is a full public release of the CEDS system.

This release was focused on fixing existing issues and adding new system capabilities as compared to the system that produced the CMIP6 data. In particular, the ability of users to add historical energy data for any country, allowing the system to more accurately reflect historical energy consumption trends. This is currently implemented for the USA, UK, and Germany. For details on this release see:

* The [release notes](https://github.com/JGCRI/CEDS/wiki/Release-Notes) for a summary of changes since the CMIP6 data release.
* [Graphs of emission differences](./documentation/Version_comparison_figures_v_2019_12_23.pdf) between this version and the CMIP6 data release documented in Hoesly et al (2018a). 
* Emissions by country and sector, archived [here](https://zenodo.org/record/3606753).

We are working on a major CEDS data update that will extend the time series to 2018, update historical assumptions where necessary, and will include gridded emissions.

Users should use the most recent version of this repository, which will include maintenance updates to address documentation or usability issues. New releases that change emissions data will be noted here and in the [release notes](https://github.com/JGCRI/CEDS/wiki/Release-Notes).
***

Documentation of CEDS assumptions and system operation, including a user guide, are available at the [CEDS project wiki](https://github.com/JGCRI/CEDS/wiki) and in the [journal paper](https://www.geosci-model-dev.net/11/369/2018/gmd-11-369-2018.html) noted below. 

Current issues with the data or system are documented in the [CEDS Issues](https://github.com/JGCRI/CEDS/issues) system in this GitHub repository. Users can submit issues using this system. These can include anomalies found in either the aggregate or gridded emissions data. Please use an appropriate tag for any submitted issues. Note that by default only unresolved issues are shown. All issues, including resolved issues, can be viewed by removing the "is:open" filter. *Issues relevant for CMIP6 data releases are tagged with a “CMIP6” label (note that issues will be closed when resolved in subsequent CEDS data releases.)*

Further information can also be found at the [project web site](http://www.globalchange.umd.edu/ceds/), including a [CMIP6 page](http://www.globalchange.umd.edu/ceds/ceds-cmip6-data/) that provides details for obtaining gridded emission datasets produced by this project for use in CMIP6.

If you plan to use the CEDS data system for a research project you are encouraged to contact [Steve Smith](mailto:ssmith@pnnl.gov) so that we can coordinate with any on-going work on the CEDS system and make sure we are not duplicating effort. CEDS is research software, and we will be happy to help and make sure that you are able to make the best possible use of this system.

CEDS has only been possible through the participation of many collaborators. Our **collaboration policy** is that collaborators who contribute data used in CEDS updates will be included as co-authors on the journal paper that describes the next CEDS major release. We particularly encourage contributions of updated emission information from countries or sectors not well represented in the data currently used in CEDS.

# Data Reference

Reference for [this data version](https://github.com/JGCRI/CEDS/wiki/Release-Notes):
Hoesly, Rachel; O'Rourke, Patrick; Braun, Caleb; Feng, Leyang; Smith, Steven J.; Pitkanen, Tyler; Seibert, Jonathan J.; Vu, Linh; Presley, Muwan; Bolt, Ryan; Goldstein, Ben; Kholod, Nazar (2019, December 23). Community Emissions Data System (Version Dec-23-2019) [Zenodo. http://doi.org/10.5281/zenodo.3592073](https://zenodo.org/record/3606753).

# Journal Papers
[Hoesly et al, Historical (1750–2014) anthropogenic emissions of reactive gases and aerosols from the Community Emissions Data System (CEDS). ](https://www.geosci-model-dev.net/11/369/2018/gmd-11-369-2018.html) _Geosci. Model Dev._ 11, 369-408, 2018a.

_Note that the paper zip file supplement contains annual emissions estimates by country and sector for the July 26, 2016 data version. The most recent data is available from the links above._

[Hoesly et al Informing energy consumption uncertainty: an analysis of energy data revisions.”](https://iopscience.iop.org/article/10.1088/1748-9326/aaebc3/meta) _Environ. Res. Lett._ 13 124023, 2018b.

# <a name="license-section"></a>License
Copyright © 2017, Battelle Memorial Institute
All rights reserved.

1.	Battelle Memorial Institute (hereinafter Battelle) hereby grants permission to any person or entity lawfully obtaining a copy of this software and associated documentation files (hereinafter “the Software”) to redistribute and use the Software in source and binary forms, with or without modification.  Such person or entity may use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and may permit others to do so, subject to the following conditions:

    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimers. 
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
    * Other than as used herein, neither the name Battelle Memorial Institute or Battelle may be used in any form whatsoever without the express written consent of Battelle.

2.	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL BATTELLE OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
