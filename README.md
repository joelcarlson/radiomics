#Radiomic Texture Analysis

The `radiomics` package is a set of tools for computing texture matrices from images, and features derived from the matrices. 

You can install the package using:


```r
devtools::install_github("joelcarlson/radiomics")
```

#Texture Matrices

In the package are functions for calculating several different types of matrices used to quantify the texture of an image in the form of a matrix.

##Gray Level Co-occurrence Matrix

The first matrix type is the gray level co-occurence matrix (GLCM). GLCMs take an image (as a matrix), an angle ("0", "45", "90", or "135"), and an integer distance, d. The axes of the GLCM are defined by the grey levels present in the image. Each pixel of the image is scanned and stored as a "reference pixel". The reference pixel is then compared to the pixel that is distance d at angle theta (where "0" degrees is the pixel to the right, "90" is the pixel above) away from the reference pixel, known as the neighbor pixel. Each time a reference value and neighbor value pair is found, the corresponding row and column of the GLCM is incremented by 1. 

A visual example shows this process. Pixels in the image are colored and labelled by grey value. The GLCM is set up such that each pixel value is represented on each axis.

<img src="http://i.imgur.com/m9MKq1I.png" height="227px" width="500px" />

We count the number of times each pair of grey levels occurs, for example, if angle = "0" and d = 1, for the grey level of 1, there is one 1:1 pair:

<img src="http://i.imgur.com/z5xFw6C.png" height="227px" width="500px" />

While there are two 1:3 pairs:

<img src="http://i.imgur.com/4L8dSgf.png" height="227px" width="500px" />

Continuing in this fashion for each grey level, we end up with a GLCM filled with counts.

By convention, we sum the GLCM and the GLCMs transpose to obtain a symmetric matrix:

<img src="http://i.imgur.com/GdVzVxN.png" height="227px" width="829px" />

This can be accomplished using the `radiomics` package as follows:


```r
library(radiomics)
image <- matrix(c(1,2,3,4,1,1,2,4,3,3,3,2,4,1,4,1), nrow=4)
glcm(image, normalize=FALSE)
```

```r
##   1 2 3 4
## 1 2 2 3 0
## 2 2 0 2 1
## 3 3 2 0 2
## 4 0 1 2 2
```

Normally, GLCMs are normalized before features are calculated, this is the default situation for the `glcm()` function.

More information about the GLCM can be found [here.](http://www.fp.ucalgary.ca/mhallbey/tutorial.htm)

##Gray Level Run Length Matrix

The GLRLM is a matrix which attempts to quantify runs of the same grey level in the image. The GLRLM is set up slightly differently than the GLCM; instead of having grey levels along the abscissa of the table, the GLRLM has run lengths.

As with the GLCM, an angle is required (one of "0", "45", "90", or "135"). Below is an example using "0", note that the image matrix is not the same as the GLCM example:

<img src="http://i.imgur.com/aTapTC1.png" height="227px" width="478x" />

For each run of a given length we count how many times that length occurs for each grey level. Here, a run length of 1 pixel occurs 3 times for the yellow pixels, and a run length of 3 occurs once for the blue pixels.

<img src="http://i.imgur.com/u87fW55.png" height="227px" width="545px" />

We complete this for all pixels in the image to obtain the final GLRLM:

<img src="http://i.imgur.com/b61vqV2.png" height="227px" width="545px" />

This can be accomplished using the radiomics package with the command:

```r
image <- matrix(c(1,2,4,2,1,3,4,4,2,2,3,4,2,3,3,4), nrow=4)
glrlm(image)
```

```r
##   1 2 3 4
## 1 0 1 0 0
## 2 3 1 0 0
## 3 2 1 0 0
## 4 0 1 1 0
```

Note the extra run length column, the number of run length columns is a function of the dimensions of the image. If the run length is possible (in this case, if there were 4 pixels in a row), there will be a column for it in the output. This output can be truncated by specifying the maximum run length you wish to search for:


```r
glrlm(image, max_run_length=3)
```

```r
##   1 2 3
## 1 0 1 0
## 2 3 1 0
## 3 2 1 0
## 4 0 1 1
```

##Gray Level Size Zone Matrix

The goal of the GLSZM is to quantify regions of contiguous pixels in the image. The GLSZM is set up in the same way as the GLRLM, except along the abscissa are size zones rather than run lengths. A size zone is defined as a collection of 9-connected pixels (ie. connected on their edges and corners) of the same grey level.

For the GLSZM we will use the same image as for the GLCM. Rather than labeling grey levels, contiguous size zones are labeled with their size:

<img src="http://i.imgur.com/vNQTAsz.png" height="227px" width="496px" />

For each color we count the number of zones of a given size, here for example, there are two red zones of size 1, a single blue zone of size 3, and a single light blue zone of size 2:

<img src="http://i.imgur.com/UezvMmX.png" height="227px" width="496px" />

GLSZMs can be calculated using the `glszm` function:


```r
image <- matrix(c(1,2,3,4,1,1,2,4,3,3,3,2,4,1,4,1), nrow=4)
glszm(image)
```

```r
##   1 2 3
## 1 2 0 1
## 2 0 0 1
## 3 1 0 1
## 4 2 1 0
```

For more information on the GLSZM see [here.](http://thibault.biz/Research/ThibaultMatrices/GLSZM/GLSZM.html)


#Calculating Features

From the texture matrices it is possible to calculate many different features. These are summarized in the following table:


|First Order   |GLCM              |GLRLM  |GLSZM |
|:-------------|:-----------------|:------|:-----|
|energy        |mean              |GLN    |SAE   |
|entropy       |variance          |HGLRE  |LAE   |
|kurtosis      |autoCorrelation   |LRE    |IV    |
|meanDeviation |cProminence       |LRHGLE |SZV   |
|skewness      |cShade            |LRLGLE |ZP    |
|uniformity    |cTendency         |LGLRE  |LIE   |
|mean          |contrast          |RLN    |HIE   |
|median        |correlation       |RP     |LISAE |
|max           |differenceEntropy |SRE    |HISAE |
|min           |dissimilarity     |SRHGLE |LILAE |
|diff          |energy            |SRLGLE |HILAE |
|var           |entropy           |-      |-     |
|RMS           |homogeneity1      |-      |-     |
|sd            |homogeneity2      |-      |-     |
|-             |IDMN              |-      |-     |
|-             |IDN               |-      |-     |
|-             |inverseVariance   |-      |-     |
|-             |maxProb           |-      |-     |
|-             |sumAverage        |-      |-     |
|-             |sumEntropy        |-      |-     |
|-             |sumVariance       |-      |-     |

Further information and mathematical definitions of these quantities can be found [here.](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0102107#s5)

In the `radiomics` package, each feature associated with a given matrix can be calculated by appending the matrix name with the feature name, separated by an underscore. For example:

First order features are calculated on the image, and are prefixed with 'calc':
```r
calc_energy(hallbey)
```

```r
## [1] 42
```

GLCM features are calculated on a glcm, and take 'glcm' as a prefix:
```r
hbGLCM <- glcm(hallbey)
glcm_contrast(hbGLCM)
```

```r
## [1] 0.5833333
```

GLRLM features are calculated on a glrlm, and take 'glrlm' as a prefix:
```r
hbGLRLM <- glrlm(hallbey)
glrlm_GLN(hbGLRLM)
```

```r
## [1] 2.25
```

GLSZM features are calculated on a glszm, and take 'glszm' as a prefix:
```r
hbGLSZM <- glszm(hallbey)
glszm_LILAE(hbGLSZM)
```

```r
## [1] 8.006944
```

##Calculating features en-masse

It is not practical to type out the exact features you wish to calculate for each image matrix you create. To remedy this situation, there is a `calc_features()` function. This function, given an image matrix and the types of features you wish to calculate (any or all of "first order", "glcm", "glrlm", "glszm", "mglszm") will create the appropriate texture matrices and calculate all the features relevent to the matrix type, outputting all of the features as an observation of a data frame. 

To note: The `calc_features()` function calculates all angles ("0", "45", "90", "135") for the GLCM and GLRLM, and then averages them together. This is to ensure rotation invariance.

```r
calc_features(hallbey)
```

```r
#  image_name n_grey glcm_d  mglszm_SAE   mglszm_LAE   mglszm_IV   mglszm_SZV  mglszm_ZP    mglszm_LIE   ...
#  hallbey         32        1          0.09805366     17.54145        0.9999678    95.82953        0.249782       0.006410295 ...
```
