# Simulation Model on the Effect of Artifact-Based and Authority-Based Coordination

## Description

The model simulates two forms of coordination (artifact-based and authority-based) represented as processes building a design structure matrix. The purpose of the model is to identify differences in the matrices as result of the coordination mechanisms. Different moderating effects are considered. The research is described in the article "How Artifact-Based and Authority-Based Coordination Affect Propagation Costs in Open Source Software Development" published in _MIS Quarterly_.

## Installation

For making the code available, follow the following steps.

* Copy the file '056_main.R' and the folder 'data/' into a local folder.
* Open the '056_main.R' in R or RStudio.
* Install the packages (if not yet done) as listed at the beginning of the script.
* Load the packages as listed at the beginning of the script after the installation commands.
* Install the package 'coordinationSimulationModel' and load it. 

## Code structure

The simulation model consists of two elements: the script file '056_main.R' and the package 'coordinationSimulationModel'.
* '056_main.R' contains the installation and loading of the packages, the simulation runs, the analysis of the simulation data, mainly in form of data visualization.
* the package 'coordinationSimulationModel' contains the functions for running the model. 

## Usage

For using the model, execute the function 'getExperiment()', it contains the parameters: 
* 'authority.v' for setting one of the coordination mechanisms.
* 'ticks' for setting the number of iterations (ticks) until the simulation run stops.
* 'composability.v' for setting the degree of contribution interdependence (see paper).
* 'm.v' for setting the number of contributions generated in each iteration. 
* 'f.v' for setting the degree of which contributions are forgotten from one iteration to the next. This parameter is irrelevant when memory of contributions is not used (as it is the case in the research publication).
* 'turbulence.v' for setting the degree of decay of functions in the DSM. This parameter is not used (in the research publication).
* 'release.v' for setting the degree of visibility (see paper).
* 'size' for setting the size of the DSM (size times size).
* 'repetitions' for setting the number of repetitions for a given parameter setting. 
* 'seed' for setting the random seed for the first run for each given parameter setting. The random seed changes with repetitions.  
* 'plot' for setting whether a plot over the iterations of each simulation run is generated or not.  

If multiple values for one or several parameters are set, they are run in sequence simulation runs. 

Alternative to running simulations, pre-ran simulations can be loaded from the 'data/' folder.

Figures from the research paper can be re-created using the '056_main.R' script. 

## Run Environment

R, RStudio

Packages required:

* plyr 1.8.9
* Hmisc 5.1-3
* rgl 1.3.1
* plot3D 1.4.1
* expm v. 1.0-0
* dplyr v. 1.1.4
* beepr v 2.0
* Rsolnp v. 1.16
* ggplot2 v. 3.5.1
* qualityTools v 1.55

## License

MIT License

Copyright (c) 2024 Michael Zaggl

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.