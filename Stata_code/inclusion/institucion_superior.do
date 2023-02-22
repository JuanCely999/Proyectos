summ total,det
generate total2=total
 recode total2 (1/86=0) (87/148=1)
 
 generate ZA11=A11
 recode ZA11 1 2=0 3 4=1
 
 generate ZA12=A12
 recode ZA12 1 2=0 3 4=1
 
 generate ZA13=A13
 recode ZA13 1 2=0 3 4=1
 
 generate ZA14=A14
 recode ZA14 1 2=0 3 4=1
 
 generate ZA15=A15
 recode ZA15 1 2=0 3 4=1
 
 generate ZA16=A16
 recode ZA16 1 2=0 3 4=1
 
 generate ZA17=A17
 recode ZA17 1 2=0 3 4=1
 
 generate ZA21=A21
 recode ZA21 1 2=0 3 4=1
 
 generate ZA22=A22
 recode ZA22 1 2=0 3 4=1
 
 generate ZA23=A23
 recode ZA23 1 2=0 3 4=1
 
 generate ZA24=A24
 recode ZA24 1 2=0 3 4=1
 
 generate ZA25=A25
 recode ZA25 1 2=0 3 4=1
 
 generate ZA26=A26
 recode ZA26 1 2=0 3 4=1
 
 generate ZB11=B11
 recode ZB11 1 2=0 3 4=1
 
  generate ZB12=B12
 recode ZB12 1 2=0 3 4=1
 
  generate ZB13=B13
 recode ZB13 1 2=0 3 4=1
 
  generate ZB14=B14
 recode ZB14 1 2=0 3 4=1
 
  generate ZB15=B15
 recode ZB15 1 2=0 3 4=1
 
  generate ZB16=B16
 recode ZB16 1 2=0 3 4=1
 
  generate ZB21=B21
 recode ZB21 1 2=0 3 4=1
 
  generate ZB22=B22
 recode ZB22 1 2=0 3 4=1
 
  generate ZB23=B23
 recode ZB23 1 2=0 3 4=1
 
  generate ZB24=B24
 recode ZB24 1 2=0 3 4=1
 
  generate ZB25=B25
 recode ZB25 1 2=0 3 4=1
 
  generate ZB26=B26
 recode ZB26 1 2=0 3 4=1
 
  generate ZB27=B27
 recode ZB27 1 2=0 3 4=1
 
  generate ZB28=B28
 recode ZB28 1 2=0 3 4=1
 
  generate ZC11=C11
 recode ZC11 1 2=0 3 4=1
 
  generate ZC12=C12
 recode ZC12 1 2=0 3 4=1
 
  generate ZC13=B13
 recode ZC13 1 2=0 3 4=1
 
  generate ZC14=C14
 recode ZC14 1 2=0 3 4=1
 
  generate ZC15=C15
 recode ZC15 1 2=0 3 4=1
 
  generate ZC16=C16
 recode ZC16 1 2=0 3 4=1
 
  generate ZC17=C17
 recode ZC17 1 2=0 3 4=1
 
  generate ZC18=C18
 recode ZC18 1 2=0 3 4=1
 
  generate ZC19=C19
 recode ZC19 1 2=0 3 4=1
 
  generate ZC110=C110
 recode ZC110 1 2=0 3 4=1
 
  generate ZC111=C111
 recode ZC111 1 2=0 3 4=1
 
 generate ZC21=C21
 recode ZC21 1 2=0 3 4=1
 
 generate ZC22=C22
 recode ZC22 1 2=0 3 4=1
 
 generate ZC23=C23
 recode ZC23 1 2=0 3 4=1
 
 generate ZC24=C24
 recode ZC24 1 2=0 3 4=1
 
 generate ZC25=C25
 recode ZC25 1 2=0 3 4=1
 *EXITO
 logit total2 ZB15 ZB16 ZB23 ZC24 ZC21 ZC15 ZA25 ZC12 
estat class
  mfx
  mfx, at (ZB15=0 ZB16=0 ZB23=0 ZC24=0 ZC21=0 ZC15=0 ZA25=0 ZC12=1)
