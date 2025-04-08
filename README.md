# snowspectraltools

Tools for processing spectral and broadband snow surface albedo from Landsat OLI and other remote sensing platforms

**General Workflow**

1. Generate Lookup Table - *Completed basic*
  + Needs to be redone accounting for different solar angles
  + Currently using [SNICAR](http://snow.engin.umich.edu/) but better options exist (e.g., DISORT)
  + eventually we will generate  a better library including both dust and grain size variations in an end member mixing model like [this](https://ieeexplore.ieee.org/document/9290428) paper.
2. Spectral Convolution - *Completed - script in repository*
  + Note that there are separate tables created for Landsat 8 and 9 as the sensor changes
3. Perform *classification* of Landsat imagery using NDGSI and lookup generated NDGSI values from modeled data
4. Calculate Radiative Forcing
  + Difference between modeled clean snow spectra and observed spectra from Landsat in the visible wavelengths
5. Validation (key)
  + ideally we gather spectrometer data in the field to validate this process
6. Reassess Process and Make Improvements
  + It is very likely that the spectral resolution of Landsat is insufficient to reclassify variations in grain size
  + We may want to rethink the methods and use an alteration of NDGSI that incorporates other bands or uses an ML model to train and predict grain size
  + We might consider looking at Senintel 2 or 3
  
  
This is a working package where we can continue to develop this workflow!

