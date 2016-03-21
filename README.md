# rCharts Basic Charts
##### Description:

This node allows you to create interactive JavaScript based visualizations as a Modeler Output.  The rCharts package for R offers R users easy access to generating data visualizations that are ready to be published on the web.  This extension makes it even easier to create data visualizations by allowing direct output from SPSS Modeler to a browser based chart.   This extension allows the creation of bar, line, or scatter plots.  It allows the specification of an x-axis variable, y-axis variable and a variable that the chart should be grouped or colored.   The height and width of the plot can be modified as well as the inclusion of necessary JavaScript libraries stored locally or through a CDN.

![Stream](https://raw.githubusercontent.com/IBMPredictiveAnalytics/rCharts_Basic_Charts/master/screenshots/stream.png)
---

##### User Interface:

This extension only has one main dialog tab for manipulating the plot output.  The dialog, shown below, has the following options.

- Chart Components
 - X Axis – this selects a field from the input dataset that will be plotted along the x-axis
 - Y Axis - this selects a field from the input dataset that will be plotted along the y-axis
 - Category (Optional Grouping Variable) – If selected then the plot will be grouped by this variable, using color.  This variable is typically categorical and often is the class variable in a classification task
 - Chart Type – The current options for rCharts Basic Charts are Line, Scatter, and Bar.
- Chart Options
 - Use CDN – If this box is checked the HTML output from this extension will contain a link to a CDN (content delivery network) which hosts the necessary javascript libraries, otherwise this will be linked to a locally hosted version.
 - Height – Height in pixels of the plot
 - Width – Width in pixels of the plot
- Output
 - Temp/Custom Directory – This extension creates an HTML when creating output.  If you want to only view or take the source code from the document generated a Temporary file is best, but if you want to the HTML, select a directory to save.


![Dialog1](https://raw.githubusercontent.com/IBMPredictiveAnalytics/rCharts_Basic_Charts/master/screenshots/dialog.png)
---
---
Requirements
----
- IBM SPSS Modeler v16 or later
- ‘R Essentials for SPSS Modeler’ plugin: [Download here][4]
-  R 2.15.x, 3.1, or 3.2 (depending on version - [use this link][6] to find the correct version)

---
Installation instructions
----
1. Download the extension: [Download][5]
2. Close IBM SPSS Modeler. Save the .cfe file in the CDB directory, located by default on Windows in "C:\ProgramData\IBM\SPSS\Modeler\version\CDB" or under your IBM SPSS Modeler installation directory.  Note: this is a hidden directory, so you need to type it in manually or copy/paste the file path.
3. Restart IBM SPSS Modeler, the node will now appear in the Output palette.

---
R Packages used
----
The R packages will be installed the first time the node is used as long as an Internet connection is available.

- [rCharts][7]
- [RCurl][8]
- [devtools][9]


---
Documentation and samples
----
- Find a PDF with the documentation of this extension in the [Documentation][2] directory
- There is a sample available in the [Example][3] directory


---
License
----

[Apache 2.0][1]


Contributors
----

  - Greg Filla ([gdfilla](https://twitter.com/gdfilla))


[1]: http://www.apache.org/licenses/LICENSE-2.0.html
[2]: https://github.com/IBMPredictiveAnalytics/rCharts_Basic_Charts/tree/master/docs
[3]: https://github.com/IBMPredictiveAnalytics/rCharts_Basic_Charts/tree/master/example
[4]: https://github.com/IBMPredictiveAnalytics/R_Essentials_Modeler/releases
[5]:https://github.com/IBMPredictiveAnalytics/rCharts_Basic_Charts/blob/master/src/rChartsBasicCharts.cfe
[6]:https://developer.ibm.com/predictiveanalytics/downloads/
[7]: http://ramnathv.github.io/rCharts/
[8]: https://cran.r-project.org/web/packages/RCurl/
[9]: https://cran.r-project.org/web/packages/devtools/
