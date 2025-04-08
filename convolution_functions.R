#
# Spectral convolution code
# 2024

# libraries
library(dplyr)




# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION Spectral Convolution - reformat!
LS.spec.convolve <- function(df_hyper, df_rsr, df_oli){
  # Landsat ls_sub is 1nm spectral resolution
  # hyspec lookup table is 10nm (or other)
  # converts to new dataframe
  #
  # LOOP
  for(bnd in 1:nrow(df_oli)){
    # band name
    nm = paste0(df_oli$Band[bnd],"_",df_oli$BN[bnd])
    print(paste("Starting",nm))
    #
    # subset hyperspectral
    hy_sub = df_hyper %>% filter(
      lambda >= df_oli[bnd,"Lower_edge_nm"] &
        lambda <= df_oli[bnd,"Upper_edge_nm"]
    ) 
    # head(hy_sub, 1500) # by 10nm ... 455, 465, etc.
    #
    # filter for only exiting hyspec
    hy_unique = unique(hy_sub$lambda)
    # subset landsat data
    ls_sub = df_rsr %>% filter(
      Wavelength >= df_oli[bnd,"Lower_edge_nm"] &
        Wavelength <= df_oli[bnd,"Upper_edge_nm"]
    ) %>% filter(Wavelength %in% hy_unique)
    head(ls_sub, 10) # by 1nm (filter)
    #
    # perform convolution
    dfj <- left_join(x=hy_sub, y=ls_sub,by=join_by("lambda"=="Wavelength")) %>% 
      group_by(grain_size) %>% 
      summarize(Ly = sum(reflectance*rsr)/sum(rsr))
    colnames(dfj)[2] <- nm
    # save
    if(bnd==1){dff = dfj}else{
      # does not check that gain sizes match
      # left_join(dff, dfj, by="grain_size")
      dff = cbind(dff, dfj[,2])
      # assign(nm, dfj[,2])
      # dff = cbind(dff, nm = get(nm))
      print("...")
    }
  }
  return(dff)
}
##################



