# cartridgeInvestigatR
Use this app to compare 3D topographical scans of cartridge cases.
An overview of the app functionality is provided below.

# App usage tutorial

## 0. Introduction

This application uses computer algorithms to process and compare scans of cartridge cases. 
A *cartridge case* is the metal casing that houses the bullet and gunpowder prior to firing. 
When a gun is fired, as the bullet moves down the barrel, the cartridge case moves backwards and slams against the back wall of the barrel (a.k.a. the breech face ) with considerable force. 
Any markings on the breech face are "stamped" into the surface of the cartridge case. 
This leaves so-called [*breech face impressions*](https://www.firearmsid.com/A_CCIDImpres.htm) that forensic examiners use to identify the gun from which a cartridge case was fired. 
Think of these impressions as analogous to a gun's "fingerprint" left on the cartridge case. 
The computer algorithms used in this app compare the breech face impressions on two cartridge cases.

The purpose of this app is to allow individuals to engage with these algorithms without needing to program. 
If you are interested in cartridge case identification, but do not have expertise in programming, specifically in the R programming language, then this app is for you. For more information about the computer algorithms used in this application, visit [https://github.com/CSAFE-ISU/cmcR](https://github.com/CSAFE-ISU/cmcR).

To use this app, you must have cartridge case scans stored on your computer as [.x3p files](https://tsapps.nist.gov/NRBTD/Home/DataFormat).
X3P (XML 3D Surface Profile) is an ISO standard file format for saving cartridge case scans. You can download example .x3p files containing cartridge case scan from the [NIST Ballistics Toolmark Research Database](https://tsapps.nist.gov/NRBTD/).

The functionality of this app encompasses three stages of the cartridge case comparison procedure: preprocessing, comparing, and scoring. 
These stages are separated into the three tabs that you can see on the left sidebar. You must complete one stage before moving onto the next. 
Following is a description of each stage and how to complete it.

## 1. Preprocessing

Breech face impressions are commony found on the annular region surrounding the firing pin impression on cartridge case primer. 
The algorithms will not work properly if regions other than the breech face impression region are left in the scan. 
As such, before comparing two cartridge cases, scans are often "preprocessed" to isolate the breech face impression region. 
The Preprocessing tab provides both manual and automatic tools to preprocess a cartridge case scan to isolate the breech face impressions.

### Import

Use this tab to upload x3p files to the application. 
Click the "Select a folder containing x3p files" button to open a file system explorer and select a folder containing x3p files. 
Once the files are successfully uploaded, you will see a visualization of the cartridge case scans below. 
At this point, if the uploaded cartridge case scans are already preprocessed to your liking, you may click the "Skip Preprocessing" button and move on to the Comparison stage. 
Otherwise, move on to the **Automatic Preprocess** or **Manual Deletion** tabs to perform preprocessing.

### Automatic Preprocess

Use this tab to apply automatic preprocessing algorithms to the uploaded x3p files. 
You may add an arbitrary number of preprocessing steps to be performed sequentially. 
Press the "Perform Preprocessing" button once you are happy with the preprocessing procedure or "Reset" to remove all steps. 
The list of possible preprocessing steps is:

- **Downsample:** Decrease the dimension of scans by sampling every [Stride] rows/columns (e.g., Stride = 2 means every other row/column is selected).
- **Crop:** Remove primer roll-of on the exterior or interior of the breech face impression (BFI) region. The function estimates the radius of the selected [Region]. This estimate can be increased or decreased by setting the [Offset] parameter to a positive or negative value, respectively. You will likely need to experiment with different offset values to find one that isolates the region of interest.
- **Level:** Remove the global trend in a scan by fitting and subtracting a conditional [Statistic] plane to the surface.
- **Erode:** Supplementary/alternative to cropping the exterior or interior of the BFI region, apply the morphological operation of erosion with a set mask [Radius] value. A larger radius leads to more erosion.
- **Filter:** Apply a low/high/bandpass Gaussian filter with set [Wavelength] cutoff(s) to the filter surface.
- **Delete:** Remove masked values from an imported scan.

### Manual Deletion

The automatic preprocessing algorithms available in this app may not work perfectly to isolate the breech face impression region in a scan.
You may wish to manually annotate a region of the scan, such as primer roll-off, to delete it from the scan.
The steps to manually annotate a scan are:

1. Select a scan to annotate from the drop-down. A plot of the selected scan will appear on the right.
2. Click and drag your cursor to draw a rectangle on the plot. A zoomed-in visualization of the selected region will appear below.
3. Click on the zoomed-in visualization to place a point. When three or more points are placed, they will be connected to form a region. Pressing "Reset Region" will remove the points for the current region. You may start a new region by drawing a new rectangle on the plot at the top of the page.
4. Once you are happy with the annotations, press the "Confirm Annotations" button to "lock" them into the scan. A 3D rendering will appear to the right showing the annotated regions in red. Pressing the "Delete Annotations" button will reset all annotations for the selected scan.
5. You can select a new scan to annotate from the drop-down or move on to the **Preprocess** tab if you are finished manually annotating. Note that selecting a new scan from the drop-down before pressing "Confirm Annotations" for the currently-selected scan will remove all annotations - remember to click "Confirm Annotations" before moving on.

Alternative to this manual annotation tab, use [FiX3P](https://talenfisher.github.io/fix3p/) to "paint" regions of the scan that you wish to change.

### Note

To complete the Preprocessing stage, you must click either the "Skip Preprocessing" button in the **Import** tab or the "Perform Preprocessing" button in the **Automatic Preprocess** tab. 
The comparison procedure will not be available if you do not click one of these two buttons.

## 2. Comparing

After preprocessing scans, the next step is to compare them and extract similarity features. 
Use this tab to automatically compare two cartridge cases and explore the distributions of the similarity features. 
The comparison algorithm used in this tab is based on the Congruent Matching Cells (CMC) method introduced in [Song (2013)](https://www.nist.gov/publications/proposed-nist-ballistics-identification-system-nbis-based-3d-topography-measurements).
Briefly, this algorithm begins by selecting a "Reference" scan to divide into a grid of cells. 
Each Reference cell is allowed to roam the surface of a second, "Target" scan to identify its matching position. 
The Target scan is rotated by a range of angles where for each angle, each Reference cell determines the translation at which it maximizes the [cross-correlation function (CCF)](https://scikit-image.org/docs/stable/auto_examples/registration/plot_register_translation.html) in the Target scan. 
Five similarity features are collected for each Reference cell and rotation: the translation at which the CCF is maximized, the maximum CCF value, and the pairwise-complete correlation between the Reference cell and the Target scan. 
Ultimately, we are interested in determining whether multiple cells come to a "consensus" on the translation and rotation at which the correlation is maximized. 
A consensus reached amongst multiple cells is evidence to suggest that the cartridge case pair matches.

### Comparison Parameters

Use this tab to set parameters for and execute the cell-based comparison procedure described above. 

- **Select Reference Scan:** choose scan to be considered the "reference." This scan will be divided into a grid of cells.
- **Select Target Scan:** choose the scan to be considered the "target." Each reference cell will be compared to this scan.
- **Compare in both directions:** specify whether the comparison procedure is to be applied in both directions (i.e., reference compared to target and vice versa)
- **Cell Grid Size:** must be a perfect square.
- **Max. Proportion of NAs per Cell:** cells that do not meet this criterion will be grayed-out (i.e., a higher alpha level) in the plots shown on the right.
- **Rotate by Estimated Angle First:** attempts to estimate the optimal rotation between the two scans and rotates the target scan (specified by Select Target Scan) before performing the cell-based comparison procedure.
- **Min. Rotation Value, Max. Rotation Value, Rotation Step Size:** The values will be used to create a grid of rotations used for the comparison. Specifically, the values will be used by the seq function as `seq(from = min, to = max, by = step).`

### Comparison Results - Summary

Use this tab to view the distribution of rotation, translation, and correlation features for each cell.

- **Top-right:** selected reference scan divided into a grid of cells.

- **Bottom-right:** selected target scan featuring the alignment of each cell that maximizes the cross-correlation function.

- **Top-left:** the translation values for each cell faceted by rotation.

- **Bottom-left:** the distribution of translation, rotation, and correlation values for each cell that maximizes the pairwise-complete correlation.

Clicking an element of one of the plots highlights elements in the other plots corresponding to the same cell index.

### Comparison Resulst - Individual Cells

After selecting a scan from the drop-down, click on one of the cells to visualize the region in the target scan to which the selected cell aligns (i.e., maximizes the CCF).

### Cell Trajectories

Select cells to create an animation of their alignment in the target scan across multiple rotations.

### Custom Cell

Draw your own cell or hand-drawn region on a reference scan and visualize where this custom cell aligns in the selected target scan.

## 3. Scoring
