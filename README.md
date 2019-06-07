# CEDS
The Community Emissions Data System (CEDS) produces consistent estimates of global air emissions species over the industrial era (1750 - present). The system is written in R and uses open-source data (with the exception of the IEA energy statistics which must be purchased from IEA). CEDS is publicly available through an [Open Source License](#license-section).

***
**Pre-release Version:** June 7, 2019. The current code and data in the repository is a pre-release version. Please feel free to explore, comment on this version, and send us any questions. Note that we will be pushing several updates to the system over the 2 weeks leading up to the first public release. These will include updates to:

* The generation of historical energy data in Module A is currently very slow during the incorporation of user-added energy data. We plan to optomize this section of the code.
* SO2, CO2, and CH4 emissions will be updated to better match the results from Hoesly et al. (2018). There may be changes to other emission species as well.
* Several other updates to code and data files, including some of the current [open Issues](https://github.com/JGCRI/CEDS/issues).
* User guide additions.

***

Documentation of CEDS assumptions and system operation, including a user guide, are available at the [CEDS project wiki](https://github.com/JGCRI/CEDS/wiki) and in the journal paper listed below. 

Current issues with the data or system are documented in the [CEDS Issues](https://github.com/JGCRI/CEDS/issues) system in this gitHub repository (current issues as of September 2017 are also listed in the "Known Issues" section of the Data and Assumptions wiki. Newer issues are on gitHub.). Users can submit issues using this system. These can include anomalies found in either the aggregate or gridded emissions data. Please use an appropriate tag for any submitted issues. Note that by default only unresolved issues are shown. All issues, including resolved issues, can be viewed by removing the "is:open" filter. Issues relevant for CMIP6 data releases are tagged with a “CMIP6” label (note that some of these will be closed if resolved in subsequent CEDS data releases.)

Further information can also be found at the [project web site](http://www.globalchange.umd.edu/ceds/), including a [CMIP6 page](http://www.globalchange.umd.edu/ceds/ceds-cmip6-data/) that provides details for obtaining gridded emission datasets produced by this project. 

Over the summer 2019 we will be working on a major update extending the time series to 2019.

If you plan to use CEDS for a research project you are encouraged to contact [Steve Smith](mailto:ssmith@pnnl.gov) so that we can coordinate with any on-going work. 

# Journal Papers
[Hoesly et al, Historical (1750–2014) anthropogenic emissions of reactive gases and aerosols from the Community Emissions Data System (CEDS). Geosci. Model Dev. 11, 369-408, 2018.](https://www.geosci-model-dev.net/11/369/2018/gmd-11-369-2018.html)

Note that the paper zip file supplement contains annual emissions estimates by country and sector.

[Hoesly et al, Informing energy consumption uncertainty: an analysis of energy data revisions.” Environ. Res. Lett. 13 124023.](https://iopscience.iop.org/article/10.1088/1748-9326/aaebc3/meta)

# <a name="license-section"></a>License
Copyright © 2017, Battelle Memorial Institute
All rights reserved.

1.	Battelle Memorial Institute (hereinafter Battelle) hereby grants permission to any person or entity lawfully obtaining a copy of this software and associated documentation files (hereinafter “the Software”) to redistribute and use the Software in source and binary forms, with or without modification.  Such person or entity may use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and may permit others to do so, subject to the following conditions:

    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimers. 
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
    * Other than as used herein, neither the name Battelle Memorial Institute or Battelle may be used in any form whatsoever without the express written consent of Battelle.

2.	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL BATTELLE OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
