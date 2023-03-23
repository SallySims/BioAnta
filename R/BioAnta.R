
#'This function estimates weight (W)
#' @param W Weight
#' @return body composition estimate.
#' @export
#'
W <- function(W) {
  #where W is weight(kg)
  return(W)
}

#' This function provides estimates of height (H)
#' @param H Height
#' @export
#'
H <- function(H) {
  # where H is height (m)
  return((H/100))
}

#' This function provides estimates of waist circumference (WC)
#' @param WC  Waist Circumference
#' @export
#'
WC <- function(WC) {
  # where WC is Waist Circumference (WC) (cm)
  return((WC))
}

#' This function provides estimates of hip circumference (HC)
#'@param HC Hip Circumference
#' @export
#'
HC <- function(HC) {
  # where HC is Hip Circumference (HC) (cm)
  return((HC))
}


#' This function provides estimates of body mass index (BMI) using W and H
#'@param BMI body mass index
#'@param W weight
#'@param H height
#' @export
#'
BMI <- function(W, H) {
  #where W is weight(kg) and H is height (m2)
  return(W / (H^2))
}

#' This function provides estimates of body shape index (ABSI) using W, H, WC
#'@param ABSI body shape index
#'@param W weight
#'@param H height
#'@param WC waist circumference
#' @export
#'
ABSI<- function(WC, H, W) {
  #where W is weight(kg) and H is height (m) and WC is waist circumference (m)
  return(((WC/100)/(((W / (H^2))^(2/3))*sqrt(H))))
}

#' This function provides estimates of adiposity volume index (AVI) using HC and WC
#'@param AVI adiposity volume index
#'@param WC waist circumference
#'@param HC hip circumference
#' @export
#'
AVI<- function(WC,HC) {
  #where H is height (m) and WC is waist circumference (cm)
  return(((2*(WC^2))+(0.7*((WC-HC)^2)))/1000)
}

#' This function provides estimates of body adiposity index (BAI) using H, HC
#'@param BAI body adiposity index
#'@param HC hip circumference
#'@param H height
#' @export
#'
BAI<- function(HC, H) {
  #where H is height (m) and HC is hip circumference (cm)
  return(((HC/((H)^1.5)))-18)
}

#' This function provides estimates of age
#'@param age age
#' @export
#'
age<- function(age) {
  #where age is age in completed years/numeric
  return(age)
}

#' This function provides estimates of bodyfat percentage  (BFperc) using age, sex, WC, H, W, HC
#'@param BFperc body fat percentage
#'@param HC hip circumference
#'@param H height
#'@param WC waist circumference
#'@param W weight
#'@param age age
#'@param sex sex
#'@export
#'
BFperc <- function(sex, age, WC, H, W, HC) {

  if (sex == "male") {
    # Male body fat percentage formula
    bf_percentage <- 495 / (1.0324 - 0.19077*log10(WC - W*0.15) + 0.15456*log10(H)) - 450 / (age + 16) + 0.29
  } else if (sex == "female") {
    # Female body fat percentage formula
    bf_percentage <- 495 / (1.29579 - 0.35004*log10(WC + HC - W*0.082) + 0.22100*log10(H)) - 450 / (age + 16) + 0.29
  } else {
    stop("Invalid sex argument. Must be either 'male' or 'female'")
  }

  # Return the calculated body fat percentage rounded to two decimal places
  return(round(bf_percentage, 2))
}

#' This function provides estimates of relative fat mass (RFM) using  sex, WC, H
#'@param RFM relative fat mass
#'@param H height
#'@param WC waist circumference
#'@param sex sex
#'@export
#'
RFM <- function(WC, H, sex) {
  # where H is height (m) and WC is waist circumference (cm)
  if (sex == "m") {
    64 - (20 * (H / WC))
  } else if (sex == "f") {
    76 - (20 * (H / WC))
  } else {
    stop("Invalid value for sex. Must be 'm' or 'f'.")
  }
}

#' This function provides estimates of lean body mass (LBM) using  sex, W, H
#'@param LBM relative fat mass
#'@param H height
#'@param W weight
#'@param sex sex
#'@export
#'
LBM <- function(W, H, sex) {
  # where H is height (m) and W is weight (kg)
  if (sex == "m") {
    ((0.328108*W)+(0.33929*H)-29.5336)
  } else if (sex == "f") {
    ((0.29569*W)+(0.41813*H)-43.2933)
  } else {
    stop("Invalid value for sex. Must be 'm' or 'f'.")
  }
}

#' This function provides estimates of body roundness index (BRI) using  WC , H
#'@param BRI body roundness index
#'@param H height
#'@param WC waist circumference
#'@export
#'
BRI <- function(WC, H) {
  # where WC is waist circumference (cm) and H is height (m)

  # Calculate hip circumference (HC) using the formula:
  HC <- 0.85 * (WC - 2 * pi)

  # Calculate BRI using the formula:
  bri <- HC / H

  # Return the BRI value rounded to two decimal places
  round(bri, 2)
}

#' This function provides estimates of broca index (BI) using  sex, W, H
#'@param broca_index_BI broca index
#'@param H height
#'@param sex sex
#'@export
#'
broca_index_BI <- function(H, sex) {
  # where height is height in cm and sex is "m" or "f"

  # Define constants for the Broca formula
  if (sex == "m") {
    c <- 100
    h <- 0.1
  } else if (sex == "f") {
    c <- 105
    h <- 0.15
  } else {
    stop("Invalid value for sex. Must be 'm' or 'f'.")
  }

  # Calculate the Broca index using the formula
  broca <- (H - c) * h

  # Return the Broca index value rounded to two decimal places
  round(broca, 2)
}

#' This function provides estimates of conicity index (CI) using  WC, W, H
#'@param CI conicity index
#'@param H height
#'@param W weight
#'@param WC waist circumference
#'@export
#'
CI <- function(WC, W, H) {
  wc_m <- WC/ 100  # Convert waist circumference from cm to m
  ci <- wc_m / (0.109 * sqrt(W / H))
  return(ci)
}

#' This function provides estimates of fat free mass (FFM) using  W,BFperc
#'@param FFM fat free mass
#'@param BFperc body fat percentage
#'@param W weight
#'@export
#'
FFM <- function(W, BFperc) {
  fat_mass <- W * (BFperc / 100)
  fat_free_mass <- W - fat_mass
  return(fat_free_mass)
}

#' This function provides estimates of fat free mass index (FFMI) using  FFM, H
#'@param FFMI relative fat mass
#'@param H height
#'@param FFM fat free mass
#'@export
#'
FFMI <- function(H, FFM) {
  ffmi <- FFM / (H ^ 2)
  return(ffmi)
}

#' This function provides estimates of hip-index(HI) using  HC, W, H
#'@param HI hip index
#'@param H height
#'@param HC hip circumference
#'@param W weight
#'@export
#'
HI <- function(W, H, HC) {
  # where height is H(cm), weight is W(KG), HC is Hip circumference (m)
  # Calculate mean weight and height
  mean_weight <- mean(W)
  mean_height <- mean(H)

  # Calculate hip index using the formula
  hip_index <- ((HC * W/ mean_weight) / ((H*100) / mean_height))

  # Return the hip index rounded to two decimal places
  return(round(hip_index, 2))
}

#' This function provides estimates of rohrer's or ponderal index (PI_or_RI) using  sex, W, H
#'@param PI_or_RI rohrer's or ponderal index
#'@param H height
#'@param W weight
#'@export
#'
PI_or_RI <- function(H, W) {
  # Calculate Rohrer's index using height (in meters) and weight (in kg)
  rohrers_index <- W / (H^(3/2))
  # Return Rohrer's index rounded to two decimal places
  return(round(rohrers_index, 2))
}

#' This function provides estimates of reciprocal ponderal index (RPI) using  sex, W, H
#'@param RPI rohrer's or ponderal index
#'@param H height
#'@param W weight
#'@export
#'
RPI <- function(H, W) {
  # Calculate RPI using height (in meters) and weight (in kg)
  rpi <- H / (W^(1/3))
  # Return RPI rounded to two decimal places
  return(round(rpi, 2))
}

#' This function provides estimates of viseral adiposity index (VAI) using  sex, WC,BMI, hdl, triglycerides
#'@param VAI visceral adiposity index
#'@param BMI body mass index
#'@param WC waist circumference
#'@param sex sex
#'@param triglycerides triglycerides
#'@param hdl hdl
#'@export
#'
VAI <- function(sex, WC, BMI, triglycerides, hdl) {
  # Calculate VAI using sex, waist circumference (in cm), BMI, triglyceride levels, and HDL cholesterol levels
  if (sex == "m") {
    vai <- (WC / (102 * sqrt(BMI))) * (triglycerides / 0.81) * (1.52 / hdl)
  } else if (sex == "f") {
    vai <- (WC / (88 * sqrt(BMI))) * (triglycerides / 1.81) * (1.31 / hdl)
  } else {
    stop("Invalid value for sex. Must be 'male' or 'female'.")
  }
  # Return VAI rounded to two decimal places
  return(round(vai, 2))
}

#' This function provides estimates of waist hip ratio (WHR) using WC, HC
#'@param WHR waist to hip ratio
#'@param HC hip circumference
#'@param WC waist circumference
#'@export
#'
WHR <- function(WC, HC) {
  # calculate WHR where WC is waist circumference (cm), HC id hip circumference (cm)
  whr <- WC / HC
  return(whr)
}

#' This function provides estimates of waist height ratio (WHTR) using  WC, H
#'@param WHtR waist to height ratio
#'@param H height
#'@param WC waist circumference
#'@export
#'
WHtR <- function(WC, H) {
  # calculate WHtR where WC is waist circumference (cm), Height is H (cm)
  whtr <- WC / (H/100)
  return(whtr)
}

#' This function provides estimates of weighted-weight index (WWI) using WC, HC, H
#'@param WWI weighted-weight index
#'@param H height
#'@param WC waist circumference
#'@param HC hip circumference
#'@export
#'
WWI <- function(WC, HC, H) {
  # calculate WWI where waist_circumference is WC (cm), HC is hip_circumference (cm))  and H is height in (cm)
  wwi <- (WC / HC) / (H/100)
  return(wwi)
}

#' This function provides estimates of waist to height.5 ratio (WHT-5R) using  WC, H
#'@param WHT_5R waist to height.5 ratio
#'@param H height
#'@param WC waist circumference
#'@export
#'
WHT_5R<- function(WC, H) {
  # calculate WWI where waist_circumference is WC (cm) and H is height in (m)
  wht5r <- WC  / (H^0.5)
  return(wht5r)
}

#' This function provides estimates of body surface area (BSA) using  W, H
#'@param BSA body surface area
#'@param H height
#'@param W weight
#'@export
#'
BSA <- function(W, H) {
  # calculate WWI where Weight is W (kg) and H is height in (m)
  bsa_value <- 0.007184 * W^(0.425) * H^(0.725)
  return(bsa_value)
}

#' This function provides estimates of hip-to-waist ratio (HWR) using  WC, HC
#'@param HWR hip-to-waist ratio
#'@param HC hip circumference
#'@param WC waist circumference
#'@export
#'
HWR <- function(HC, WC) {
  ##Hip-to-Waist Ratio (HWR):
  hwr_value <- HC / WC
  return(hwr_value)
}

#' This function provides estimates of Sagittal Abdominal Diameter (SAD) using  WC, H
#'@param SAD Sagittal Abdominal Diameter
#'@param H height
#'@param WC waist circumference
#'@export
#'
SAD <- function(WC, H) {
  ##Sagittal Abdominal Diameter (SAD):
  sad_value <- WC/ (0.5 * H)
  return(sad_value)
}

#' This function provides estimates of Abdominal Circumference Index (ACI) using  WC, HC, H
#'@param ACI Abdominal Circumference Index
#'@param H height
#'@param WC waist circumference
#'@param HC hip circumference
#'@export
#'
ACI <- function(WC, HC, H) {
  ###Abdominal Circumference Index (ACI):
  aci_value <- (WC * H) / HC
  return(aci_value)
}

#' This function provides estimates of Total Body Fat Percentage (TBF) using  W, H, age, sex, WC, HC
#'@param TBF	 Total Body Fat Percentage
#'@param H height
#'@param W weight
#'@param WC waist circumference
#'@param HC hip circumference
#'@param age age
#'@param sex sex
#'@export
#'
TBF <- function(W, H, age, sex, WC, HC) {
  ##Total Body Fat Percentage (TBF%):
  # calculate WWI where Waist circumfernce is WC (cm), hip circumfernce HC (cm), Weight is W (cm) and Height is H (m)
  ##Note: the TBF% function uses the HWR function from earlier.
  if(sex == "male"){
    tbf_value <- (1.20 * HWR(WC, HC)) + (0.23 * age) - (10.8 * 1) - 5.4
  } else {
    tbf_value <- (1.20 * HWR(HC, HC)) + (0.23 * age) - (10.8 * 0) - 5.4
  }
  return(tbf_value)
}

#' This function provides estimates of Body Fat Mass (BFM) using  W, TBF
#'@param BFM Body Fat Mass
#'@param TBF  total body fat percentage
#'@param W weight
#'@export
#'
BFM <- function(W, TBF) {
  # calculate WWI where  Weight is W (cm)
  ##Note: the BFM function uses the TBF% function from earlier.
  bfm_value <- W * (TBF/ 100)
  return(bfm_value)
}

#' This function provides estimates of Body Volume Index (BVI) using  W,WC, HC, H
#'@param BVI Body Volume Index
#'@param H height
#'@param W weight
#'@param WC waist circumference
#'@param HC hip circumference
#'@export
#'
BVI <- function(W,H, WC, HC) {
  ###Body Volume Index (BVI):
  # calculate WWI where Waist circumfernce is WC (cm), hip circumfernce HC (cm), Weight is W (cm) and Height is H (m)
  bvi_value <- 0.7 * W / (H^(2.5)) * ((WC * HC)^(1/3))
  return(bvi_value)
}

#' This function provides estimates of Body Density (BD) using  W, H, WC, HC, age, sex
#'@param BD Body Density
#'@param H height
#'@param W weight
#'@param WC waist circumference
#'@param HC hip circumference
#'@param age age
#'@param sex sex
#'@export
#'
BD <- function(W,H, WC, HC, age, sex) {
  ##Body Density (BD):
  # calculate WWI where Waist circumfernce is WC (cm), hip circumfernce HC (cm), Weight is W (cm) and Height is H (m)
  if(sex == "m"){
    bd_value <- 1.1093800 - (0.0008267 * W) + (0.0000016 *W^(2)) - (0.0002574 * age) + (0.0000024 * age^(2)) + (0.0000956 * H) - (0.0000018 * H^(2)) + (0.0000134 * WC) + (0.0000618 * HC) - (0.0000054 * H* WC) + (0.0000057 * H * HC) - (0.0000022 * W * WC) + (0.0000027 * W * HC) + (0.0000421 * age * WC) - (0.0000095 * age * HC)
  } else {
    bd_value <- 1.0994921 - (0.0009929 * W) + (0.0000023 * W^(2)) - (0.0001392 * age) + (0.0000016 * age^(2)) + (0.0001656 * H) - (0.0000015 * H^(2)) + (0.0000560 * WC) + (0.0001160 * HC) - (0.0000021 * H* WC) + (0.0000032 * H* HC) - (0.0000020 * W* WC) + (0.0000026 * W * HC) + (0.0000497 * age * WC) - (0.0000112 * age * HC)
  }
  return(bd_value)
}

#' This function provides estimates of Fat-Free Body Density (FFBD) using  BD, TBF
#'@param BD Body Density
#'@param TBF total body fat percentage
#'@export
#'
FFBD <- function(BD, TBF) {
  ##:
  # calculate WWI where Waist circumfernce is WC (cm), hip circumfernce HC (cm), Weight is W (cm) and Height is H (m)
  ##Note: the FFBD function uses the BD and TBF% function from earlier.
  ffbd_value <- BD / (1 - (TBF / 100))
  return(ffbd_value)
}

#' This function provides estimates of Android-to-Gynoid Fat Ratio (AGR) using  WC, HC
#'@param AGR Android-to-Gynoid Fat Ratio
#'@param WC waist circumference
#'@param HC hip circumference
#'@export
#'
AGR <- function(WC, HC) {
  # calculate WWI where Waist circumference is WC (cm), hip circumference HC (cm)
  agr_value <- WC / HC
  return(agr_value)
}


#' This function provides estimates of BMI Standard Deviation Score (BMISDS) using  BMI, age,sex
#'@param BMISDS BMI Standard Deviation Score
#'@param BMI body mass index
#'@param sex sex
#'@export
#'
BMISDS <- function(BMI, sex) {
  # calculate BMI- SDS uing BMI and sex
  if(sex == "m") {
    lms_params <- c(0.74283, 15.0896, 0.1059)
  } else {
    lms_params <- c(0.74283, 15.0896, 0.1059)
  }
  l <- lms_params[1]
  m <- lms_params[2]
  s <- lms_params[3]
  bmi_sds_value <- ((BMI / m)^l - 1) / (l * s)
  return(bmi_sds_value)
}


#' This function provides estimates of Z-score for BMI (BMIZ) using  BMI, sex
#'@param BMIZ BMI Z-score
#'@param BMI body mass index
#'@param sex sex
#'@export
#'
BMIZ <- function(BMI, sex) {
## calculate BMI-Z using BMI, sex
   if(sex == "m") {
    lms_params <- c(0.74283, 15.0896, 0.1059)
  } else {
    lms_params <- c(0.74283, 15.0896, 0.1059)
  }
  l <- lms_params[1]
  m <- lms_params[2]
  s <- lms_params[3]
  bmi_zscore_value <- (BMI - m) / (s * l)
  return(bmi_zscore_value)
}

#' This function provides estimates of Age-Adjusted BMI(AABMI) using  BMI, age
#'@param AABMI Age Adjusted BMI
#'@param BMI body mass index
#'@param age age
#'@export
#'
AABMI <- function(BMI, age) {
  aa_bmi_value <- BMI + (0.046 * age) - 2.33
  return(aa_bmi_value)
}


#' This function provides estimates of Body Weight (ABW) using  W,H,sex
#'@param ABW Body Weight
#'@param H height
#'@param W weight
#'@param sex sex
#'@export
#'
ABW<- function(W, H, sex) { if(sex == "m") {
    lbm_value <- (0.32810 * W) + (0.33929 * H) - 29.5336
  } else {
    lbm_value <- (0.29569 * W) + (0.41813 * H) - 43.2933
  }
  abw_value <- lbm_value / (1 - 0.01 * 25)
  return(abw_value)
}

#' This function provides estimates of Ideal Body Weight (IBW) using  H,sex
#'@param IBW Ideal Body weight
#'@param H height
#'@param sex sex
#'@export
#'
IBW <- function(H, sex) {

  # calculate WWI where Waist circumfernce is WC (cm), hip circumfernce HC (cm), Weight is W (cm) and Height is H (m)
  if(sex == "m") {
    ibw_value <- 50 + 2.3 * ((H / 2.54) - 60)
  } else {
    ibw_value <- 45.5 + 2.3 * ((H / 2.54) - 60)
  }
  return(ibw_value)
}

#' This function provides estimates of Adjusted Ideal Body Weight (AIBW) using H,sex
#'@param AIBW Adjusted Ideal Body Weight
#'@param H height
#'@param sex sex
#'@export
#'
AIBW<- function(H, sex) {

  # calculate WWI where Waist circumfernce is WC (cm), hip circumfernce HC (cm), Weight is W (cm) and Height is H (m)
  ibw_value <- IBW(H, sex)
  aibw_value <- ibw_value + 0.4 * (W - ibw_value)
  return(aibw_value)
}

#' This function provides estimates of Relative Weight (RW) using  W, H,sex
#'@param RW Relative Weight
#'@param H height
#'@param W weight
#'@param sex sex
#'@export
#'
RW<- function(W, H, sex) {

  # calculate WWI where Waist circumfernce is WC (cm), hip circumfernce HC (cm), Weight is W (cm) and Height is H (m)
  if(sex == "m") {
    rw_value <- (W / IBW(H, sex)) * 100
  } else {
    rw_value <- (W / IBW(H, sex))* 100
  }
  return(rw_value)
}

#' This function provides estimates of Lean Body Mass Index (LBMI) using  LBM, H
#'@param LBMI Lean Body Mass Index
#'@param H height
#'@param LBM Lean body mass
#'@export
#'
LBMI<- function(LBM, H) {
  lbmi <- (LBM / H^2) + (6.1 * (1.8 - H))
  return(lbmi)
}

#' This function provides estimates of Lean Body Mass Index (LBMI_sex) by sex using LBM, H,sex
#'@param LBMI_sex Lean Body Mass Index by sex
#'@param H height
#'@param LBM lean body mass
#'@param sex sex
#'@export
#'
LBMI_sex<- function(LBM, H, sex) {
  if (sex == "m") {
    lbmi <- (LBM / H^2) + (3.8 * (1.8 - H))
  } else if (sex == "f") {
    lbmi <- (LBM/ H^2) + (7.9 * (1.8 - H))
  } else {
    stop("Invalid sex. Please enter 'm' or 'f'.")
  }
  return(lbmi)
}


#' This function provides estimates of Handgrip Strength-to-Body Weight Ratio using  handgrip_strength, W
#'@param hgs_to_bw_ratio Handgrip Strength-to-Body Weight Ratio
#'@param handgrip_strength handgrip_strength
#'@param W weight
#'@export
#'
hgs_to_bw_ratio <- function(handgrip_strength, W) {

  # calculate WWI where  Weight is W (cm) and handgrip strength
  hgs_to_bw_ratio_value <- handgrip_strength /W
  return(hgs_to_bw_ratio_value)
}

#' This function provides estimates of Mid-Arm Muscle Circumference (MAMC) using  midarm_circumference, triceps_skinfold_thickness
#'@param MAMC Handgrip Strength-to-Body Weight Ratio
#'@param midarm_circumference midarm_circumference
#'@param triceps_skinfold_thickness triceps_skinfold_thickness
#'@export
#'
MAMC <- function(midarm_circumference, triceps_skinfold_thickness) {

  # calculate  where  Weight is W (cm) and handgrip strength
  mamc_value <- midarm_circumference - (3.14 * triceps_skinfold_thickness)
  return(mamc_value)
}


#' This function provides estimates of Arm Muscle Area (AMA) using midarm_circumference, triceps_skinfold_thickness
#'@param AMA Arm Muscle Area
#'@param midarm_circumference midarm_circumference
#'@param triceps_skinfold_thickness triceps_skinfold_thickness
#'@export
#'
AMA <- function(midarm_circumference, triceps_skinfold_thickness) {

  ama_value <- (midarm_circumference^2 / (4 * 3.14)) - (0.785 * triceps_skinfold_thickness^2)
  return(ama_value)
}

#' This function provides estimates of Triceps Skinfold Thickness (TST) using triceps_skinfold_measurement
#'@param TST Triceps Skinfold Thickness
#'@param triceps_skinfold_measurement triceps_skinfold_measurement
#'@export
#'
TST <- function(triceps_skinfold_measurement) {
  triceps_skinfold_measurement<-triceps_skinfold_measurement*1
  return(triceps_skinfold_measurement)
}

#' This function provides estimates of Biceps Skinfold Thickness (BST) using biceps_skinfold_measurement
#'@param BST Biceps Skinfold Thickness
#'@param biceps_skinfold_measurement biceps_skinfold_measurement
#'@export
#'

BST<- function(biceps_skinfold_measurement) {
  return(biceps_skinfold_measurement)
}


#' This function provides estimates of Subscapular Skinfold Thickness (SST) using subscapular_skinfold_measurement
#'@param SST Subscapular Skinfold Thickness
#'@param subscapular_skinfold_measurement subscapular_skinfold_measurement
#'@export
#'

SST <- function(subscapular_skinfold_measurement) {
  return(subscapular_skinfold_measurement)
}

#' This function provides estimates of Suprailiac Skinfold Thickness (SIT) using suprailiac_skinfold_measurement
#'@param SIT Suprailiac Skinfold Thickness
#'@param suprailiac_skinfold_measurement suprailiac_skinfold_measurement
#'@export
#'

SIT<- function(suprailiac_skinfold_measurement) {
  return(suprailiac_skinfold_measurement)
}

#' This function provides estimates of Abdominal Skinfold Thickness (AST) using triceps, subscapular, suprailiac
#'@param AST Abdominal Skinfold Thickness
#'@param triceps triceps
#'@param subscapular subscapular
#'@param suprailiac suprailiac
#'@export
#'
AST <- function(triceps, subscapular, suprailiac) {
  ast_value <- subscapular + suprailiac
  return(ast_value)
}

#' This function provides estimates of Thigh Skinfold Thickness (TSTN) using thigh
#'@param TSTN Thigh Skinfold Thickness
#'@param thigh thigh
#'@export
#'
TSTN <- function(thigh) {
  return(thigh)
}

#' This function provides estimates of Calf Skinfold Thickness (CST) using calf
#'@param CST Calf Skinfold Thickness
#'@param calf calf
#'@export
#'

CST<- function(calf) {
  calf <- 1*calf
  return(calf)
}


#' This function provides estimates of Sum of Skinfold Thicknesses (SSTK) using triceps, subscapular, suprailiac, thigh, calf
#'@param SSTK Sum of Skinfold Thicknesses
#'@param triceps triceps
#'@param subscapular subscapular
#'@param suprailiac suprailiac
#'@param thigh thigh
#'@param calf calf
#'@export
#'

SSTK <- function(triceps, subscapular, suprailiac, thigh, calf) {
  sst_value <- triceps + subscapular + suprailiac + thigh + calf
  return(sst_value)
}


#' This function provides estimates of Skinfold Fat Percentage (SFPerc) using sex, age, triceps, abdominal,subscapular, suprailiac
#'@param SFPerc Skinfold Fat Percentage
#'@param triceps triceps
#'@param subscapular subscapular
#'@param suprailiac suprailiac
#'@param abdominal abdominal
#'@param age age
#'@param sex sex
#'@export
#'
SFPerc<- function(sex, triceps, age, subscapular, suprailiac,abdominal) {
  if(sex == "m") {
    sfmm <- triceps + subscapular + suprailiac
    sfperc <- 495 / (1.112 - 0.00043499*sfmm + 0.00000055*sfmm^2 - 0.00028826*age) - 450
  } else if(sex == "f") {
    sfmm <- triceps + subscapular + suprailiac + abdominal
    sfperc <- 495 / (1.097 - 0.00046971*sfmm + 0.00000056*sfmm^2 - 0.00012828*age) - 450
  } else {
    stop("Invalid sex value")
  }
  return(sfperc)
}


#' This function provides estimates of  Skinfold Density (sfd) using triceps, subscapular, suprailiac
#'@param sfd  Skinfold Density
#'@param triceps triceps
#'@param subscapular subscapular
#'@param suprailiac suprailiac
#'@export
#'
# function to calculate Skinfold Density (SFD)
sfd <- function(triceps, subscapular, suprailiac) {
  sfd_value <- 1.10938 - (0.0008267 * (triceps + subscapular + suprailiac)) + (0.0000016 * (triceps + subscapular + suprailiac)^2) - (0.0002574 * age)
  return(sfd_value)
}


#' This function provides estimates of  Body Cell Mass (BCM) using sex, H,W
#'@param BCM Body Cell Mass
#'@param H height
#'@param W weight
#'@param sex sex
#'@export
#'
BCM <- function(sex, H, W){
  if(sex == "m"){
    bcm <- (H*9.058/100) + (W*0.32) - 4.5
  } else {
    bcm <- (H*7.267/100) + (W*0.267) - 0.6
  }
  return(bcm)
}

#' This function provides estimates of Extracellular Mass (ECM) using BCM,W
#'@param ECM Extracellular Mass
#'@param BCM Body Cell Mass
#'@param W weight
#'@export
#'
# Calculate
ECM <- function(W, BCM){
  ecm <- W - BCM
  return(ecm)
}

#' This function provides estimates of Total Body Water (TBW) using TBW,W
#'@param TBW Total Body Water
#'@param W weight
#'@export
#'

TBW <- function(W){
  tbw<-W* 0.6
  return(tbw)
}

#' This function provides estimates of Intracellular Water (ICW) using TBW,ECM
#'@param ICW Intracellular Water
#'@param TBW Total Body Water
#'@param ECM Extracellular Mass
#'@export
#'
ICW<- function(TBW, ECM){
  icw <- TBW - ECM
  return(icw)
}

#' This function provides estimates of Extracellular Water (ECW) using TBW,ICW
#'@param ECW Extracellular Water
#'@param ICW Intracellular Water
#'@param TBW Total Body Water
#'@export
#'
ECW<- function(TBW, ICW){
  ecw <- TBW - ICW
  return(ecw)
}

#' This function provides estimates of Basal Metabolic Rate (BMR) using W, H, age, sex
#'@param BMR Basal Metabolic Rate
#'@param H height
#'@param W weight
#'@param sex sex
#'@param age age
#'#'@export
#'
BMR <- function(W, H, age, sex){
  if(sex == "m"){
    bmr <- 88.362 + (13.397 * W) + (4.799 * H) - (5.677 * age)
  }else{
    bmr <- 447.593 + (9.247 * W) + (3.098 * H) - (4.330 * age)
  }

  return(bmr)
}

#' This function provides estimates of Resting Metabolic Rate (RMR) using BMR
#'@param RMR Resting Metabolic Rate
#'@param BMR Basal Metabolic Rate
#'#'@export
#'
# Calculate Resting Metabolic Rate (RMR)
RMR <- function(BMR){
  rmr<- BMR * 1.2

  return(rmr)
}

#' This function provides estimates of Energy Requirement (EER) using bmr, tef, pa, pal
#'@param EER Energy Requirement
#'@param bmr Basal Metabolic Rate
#'@param tef Thermic Effect of Food
#'@param pa Physical Activity Level
#'@param pal sex
#'#'@export
#'

EER <- function(bmr, tef, pa, pal){
  eer <- bmr * tef * pa * pal
  return(eer)
}

#' This function provides estimates of Harris-Benedict Equation (HBE)(is same as BMR) using  W,H,age,sex
#'@param HBE Harris-Benedict Equation (HBE)(is same as BMR)
#'@param H height
#'@param W weight
#'@param age age
#'@param sex sex
#'@export
#'

HBE <- function(W, H, age, sex){
  if(sex== "m"){
    bmr <- 88.362 + (13.397 * W) + (4.799 * H) - (5.677 * age)
  }else{
    bmr <- 447.593 + (9.247 * W) + (3.098 * H) - (4.330 * age)
  }

  return(bmr)
}

#' This function provides estimates of Mifflin-St Jeor Equation (MSJ) using  W,H,age,sex
#'@param MSJ Mifflin-St Jeor Equation
#'@param H height
#'@param W weight
#'@param age age
#'@param sex sex
#'@export
#'
MSJ <- function(W, H, age, sex){
  if(sex == "m"){
    bmr <- (10 * W) + (6.25 * H) - (5 * age) + 5
  }else{
    bmr <- (10 *W) + (6.25 * H) - (5 * age) - 161
  }

  return(bmr)
}

#' This function provides estimates of Katch-McArdle Equation (KMA) using  LBM
#'@param KMA Katch-McArdle Equation
#'@param LBM lean body mass
#'@export
#'

KMA <- function(LBM){
  kma <- 370 + (21.6 * LBM)
  return(kma)
}

#' This function provides estimates of Body Mass Index for Age (BMIFORAGE)using  age,sex, BMI
#'@param BMIFORAGE Body Mass Index for Age
#'@param BMI body mass index
#'@param age age
#'@param sex sex
#'@export
#'

BMIFORAGE <- function(age, sex, BMI) {
  if (sex == "m") {
    L <- -1.076 + 1.183 * age - 0.104 * age^2 + 0.046 * age^3
    m <- 19.171 - 0.138 * age
    s <- 0.089
  } else {
    L <- -1.550 + 0.982 * age + 0.221 * age^2 - 0.023 * age^3
    m <- 18.620 - 0.078 * age
    s <- 0.082
  }

  BMIZ <- (BMI - m) / (s * L)

  return(BMIZ)
}

#' This function provides estimates of ## Mid-Parental Height (MPH) using  mother_height, father_height, sex
#'@param MPH ## Mid-Parental Height
#'@param mother_height mother height
#'@param father_height father height
#'@param sex sex
#'@export
#'
MPH <- function(mother_height, father_height, sex) {
  if (sex == "m") {
    mph <- (mother_height + father_height + 13) / 2
  } else {
    mph <- (mother_height + father_height - 13) / 2
  }
  return(mph)
}


#' This function provides estimates of Target Height (TH) using  sex, father_height, mother_height
#'@param TH Target Height
#'@param mother_height mother height
#'@param father_height father height
#'@param sex sex
#'@export
#'
TH <- function(sex, father_height, mother_height) {
  if (sex == "m") {
    target_height <- (father_height + mother_height + 13) / 2
  } else {
    target_height <- (father_height + mother_height - 13) / 2
  }
  return(target_height)
}

#' This function provides estimates of Growth Velocity (GV) using H, times
#'@param GV Growth Velocity
#'@param H height
#'@param times times
#'@export
#'

GV <- function(H, times) {
  if (length(H) != length(times)) {
    stop("The lengths of the 'heights' and 'times' vectors must be the same.")
  }
  velocity <- numeric(length(H) - 1)
  for (i in 1:(length(H) - 1)) {
    velocity[i] <- (H[i + 1] - H[i]) / (times[i + 1] - times[i])
  }
  return(velocity)
}

#' This function provides estimates of Body Mass Index Velocity (BMIV) using weights, heights, times
#'@param BMIV Body Mass Index Velocity
#'@param heights heights
#'@param times times
#'@param weights weights
#'@export
#'

BMIV <- function(weights, heights, times) {
  if (length(weights) != length(heights) || length(weights) != length(times)) {
    stop("The lengths of the 'weights', 'heights', and 'times' vectors must be the same.")
  }
  bmi <- weights / (heights / 100)^2
  bmi_velocity <- numeric(length(weights) - 1)
  for (i in 1:(length(weights) - 1)) {
    bmi_velocity[i] <- (bmi[i + 1] - bmi[i]) / (times[i + 1] - times[i])
  }
  return(bmi_velocity)
}

#' This function provides estimates of Height Velocity (HV) using heights, times
#'@param HV Height Velocity
#'@param heights heights
#'@param times times
#'@export
#'

HV <- function(heights, times) {
  if (length(heights) != length(times)) {
    stop("The lengths of the 'heights' and 'times' vectors must be the same.")
  }
  velocity <- numeric(length(heights) - 1)
  for (i in 1:(length(heights) - 1)) {
    velocity[i] <- (heights[i + 1] - heights[i]) / (times[i + 1] - times[i])
  }
  return(velocity)
}

#' This function provides estimates of Body Mass Index Standard Deviation Score Velocity (BMISDSV) using bmi_sds_t1, bmi_sds_t2, t1_age, t2_age
#'@param BMISDSV Body Mass Index Standard Deviation Score Velocity
#'@param bmi_sds_t1 bmi_sds_t1
#'@param bmi_sds_t2 bmi_sds_t2
#'@param t1_age t1_age
#'@param t2_age t2_age
#'@export
#'
BMISDSV <- function(bmi_sds_t1, bmi_sds_t2, t1_age, t2_age) {
  bmi_sds_v <- (bmi_sds_t2 - bmi_sds_t1) / (t2_age - t1_age)
  return(bmi_sds_v)
}

#' This function provides estimates of Body Mass Index-for-age Standard Deviation Score Velocity (BMIZ-V) using bmiz_t1, bmiz_t2, t1_age, t2_age
#'@param bmiz_V Body Mass Index-for-age Standard Deviation Score Velocity
#'@param bmiz_t1 bmiz_t1
#'@param bmiz_t2 bmiz_t2
#'@param t1_age t1_age
#'@param t2_age t2_age
#'@export
#'
bmiz_V <- function(bmiz_t1, bmiz_t2, t1_age, t2_age) {
  bmiz_v <- (bmiz_t2 - bmiz_t1) / (t2_age - t1_age)
  return(bmiz_v)
}

#' This function provides estimates of Body Mass Index-for-length Standard Deviation Score Velocity (BMI-L-SDS-V) using bmi_l_sds_t1, bmi_l_sds_t2, t1_age, t2_age
#'@param BMI_l_SDS_V Body Mass Index-for-length Standard Deviation Score Velocity
#'@param bmi_l_sds_t1 bmi_l_sds_t1
#'@param bmi_l_sds_t2 bmi_l_sds_t2
#'@param t1_age t1_age
#'@param t2_age t2_age
#'@export
#'
BMI_l_SDS_V <- function(bmi_l_sds_t1, bmi_l_sds_t2, t1_age, t2_age) {
  bmi_l_sds_v <- (bmi_l_sds_t2 - bmi_l_sds_t1) / (t2_age - t1_age)
  return(bmi_l_sds_v)
}

#' This function provides estimates of Body Mass Index-for-weight Standard Deviation Score Velocity (BMI-W-SDS-V) using bmi_w_sds_t1, bmi_w_sds_t2, t1_age, t2_age
#'@param BMI_W_SDS_V Body Mass Index-for-weight Standard Deviation Score Velocity
#'@param bmi_w_sds_t1 bmi_w_sds_t1
#'@param bmi_w_sds_t2 bmi_w_sds_t2
#'@param t1_age t1_age
#'@param t2_age t2_age
#'@export
#
BMI_W_SDS_V <- function(bmi_w_sds_t1, bmi_w_sds_t2, t1_age, t2_age) {
  bmi_w_sds_v <- (bmi_w_sds_t2 - bmi_w_sds_t1) / (t2_age - t1_age)
  return(bmi_w_sds_v)
}


##Biomarkers Estimation


#' This function provides estimates of Atherogenic Coefficient (AC) using total_chol_mgdl, HDL_mgdl
#'@param AC  Atherogenic Coefficient (AC)
#'@param total_chol_mgdl, total cholesterol (mgdl)
#'@param HDL_mgdl high density liproproten (mgdl)
#'@export
#

AC <- function(total_chol_mgdl, HDL_mgdl) {
  # Check if the input values are numeric
  if (!is.numeric(total_chol_mgdl) || !is.numeric(HDL_mgdl) || !is.numeric(HDL_mgdl)) {
    stop("Input values must be numeric.")
  }

  # Check if the input values are positive
  if (total_chol_mgdl <= 0 || HDL_mgdl <= 0 || HDL_mgdl <= 0) {
    stop("Input values must be positive.")
  }

  # Calculate the atherogenic coefficient
  ac <- (total_chol_mgdl - HDL_mgdl) / HDL_mgdl

  # Return the result
  return(ac)
}


#' This function provides estimates of Atherogenic Index of Plasma (AIP) using TG_mgdl, HDL_mgdl
#'@param AIP  Atherogenic Index of Plasma
#'@param TG_mgdl, triglycerides (mgdl)
#'@param HDL_mgdl high density liproprotein (mgdl)
#'@export
#

AIP <- function(TG_mgdl, HDL_mgdl) {
  # Check if the input values are numeric
  if (!is.numeric(TG_mgdl) || !is.numeric(HDL_mgdl)) {
    stop("Input values must be numeric.")
  }

  # Check if the input values are positive
  if (TG_mgdl <= 0 || HDL_mgdl <= 0) {
    stop("Input values must be positive.")
  }

  # Calculate the AIP
  aip <- log10(TG_mgdl / HDL_mgdl)

  # Return the result
  return(aip)
}

#' This function provides estimates of Cardiovascular Risk Ratio (CRRorCRI I) using total_chol_mgdl, HDL_mgdl
#'@param CRRorCRII  Cardiovascular Risk Ratio (CRR/CRI I)
#'@param total_chol_mgdl, total cholesterol (mgdl)
#'@param HDL_mgdl high density i (mgdl)
#'@export
#
CRRorCRII <- function(total_chol_mgdl, HDL_mgdl) {
  # Check if the input values are numeric
  if (!is.numeric(total_chol_mgdl) || !is.numeric(HDL_mgdl)) {
    stop("Input values must be numeric.")
  }

  # Check if the input values are positive
  if (total_chol_mgdl <= 0 || HDL_mgdl <= 0) {
    stop("Input values must be positive.")
  }

  # Calculate the CRR/CRI I
  crr_cri <- total_chol_mgdl / HDL_mgdl

  # Return the result
  return(crr_cri)
}

#' This function provides estimates of Cardiovascular Risk Index II (LHRorCRIII) or LDL-HDL Ratio (LHR) using total_chol_mgdl, LDL_mgdl, HDL_mgdl
#'@param LHRorCRIII  Cardiovascular Risk Index II (CRI II) or LDL-HDL Ratio (LHR)
#'@param total_chol_mgdl, total cholesterol (mgdl)
#'@param HDL_mgdl high density liproprotein (mgdl)
#'@param LDL_mgdl low high density liproprotein (mgdl)
#'@export
#
LHRorCRIII <- function(total_chol_mgdl, LDL_mgdl, HDL_mgdl) {
  # Check if the input values are numeric
  if (!is.numeric(total_chol_mgdl) || !is.numeric(LDL_mgdl) || !is.numeric(HDL_mgdl)) {
    stop("Input values must be numeric.")
  }

  # Check if the input values are positive
  if (total_chol_mgdl <= 0 || LDL_mgdl <= 0 || HDL_mgdl <= 0) {
    stop("Input values must be positive.")
  }

  # Calculate the CRI II or LHR
  cri_ii_lhr <- LDL_mgdl / HDL_mgdl

  # Return the result
  return(cri_ii_lhr)
}

#' This function provides estimates of Lipid Accumulation Product (LAP) by sex using sex, WC, triglycerides_mgdl
#'@param LAP  Lipid Accumulation Product by sex
#'@param triglycerides_mgdl, triglycerides (mgdl)
#'@param WC waist circumference
#'@param sex sex
#'@export
#
LAP <- function(sex, WC, triglycerides_mgdl) {

  # Calculate the LAP based on sex
  if (sex == "m") {
    lap <- (WC - 65) * triglycerides_mgdl
  } else if (sex == "f") {
    lap <- (WC- 58) * triglycerides_mgdl
  } else {
    stop("Invalid sex. Please enter 'm' or 'f'.")
  }

  # Return the result
  return(lap)
}

#' This function provides estimates of  LDL using total_chol, HDL_chol, triglycerides
#'@param LDL  low-density lipoprotein cholesterol
#'@param total_chol, total cholesterol (mgdl)
#'@param HDL_chol high density liproprotein chol (mgdl)
#'@param triglycerides triglycerides (mgdl)
#'@export
#

LDL <- function(total_chol, HDL_chol, triglycerides) {
  # Check if the input values are numeric
  if (!is.numeric(total_chol) || !is.numeric(HDL_chol) || !is.numeric(triglycerides)) {
    stop("Input values must be numeric.")
  }

  # Check if the input values are positive
  if (total_chol <= 0 || HDL_chol <= 0 || triglycerides <= 0) {
    stop("Input values must be positive.")
  }

  # Calculate the LDL cholesterol
  ldl <- total_chol - HDL_chol - (0.2 * triglycerides)

  # Return the result
  return(ldl)
}


#' This function provides estimates of  Triglyceride HDL Ratio (THR) using triglycerides, HDL
#'@param THR   Triglyceride HDL Ratio
#'@param HDL high density liproprotein (mgdl)
#'@param triglycerides triglycerides (mgdl)
#'@export
#'

THR <- function(triglycerides, HDL) {
  # Check if the input values are numeric
  if (!is.numeric(triglycerides) || !is.numeric(HDL)) {
    stop("Input values must be numeric.")
  }

  # Check if the input values are positive
  if (triglycerides <= 0 || HDL<= 0) {
    stop("Input values must be positive.")
  }

  # Calculate the THR
  thr <- triglycerides / HDL

  # Return the result
  return(thr)
}


#' This function provides estimates of Triglycerides glucose index (TyG) using glucose, triglycerides
#'@param TyG  Triglycerides Glucose Index
#'@param glucose glucose
#'@param triglycerides triglycerides (mgdl)
#'@export
#'

TyG <- function(glucose, triglycerides) {
  # Check if the input values are numeric
  if (!is.numeric(glucose) || !is.numeric(triglycerides)) {
    stop("Input values must be numeric.")
  }

  # Check if the input values are positive
  if (glucose <= 0 || triglycerides <= 0) {
    stop("Input values must be positive.")
  }

  # Calculate the TyG
  tyg <- log(triglycerides + 0.5 * glucose) * 1.17 + 4.63

  # Return the result
  return(tyg)
}


#' This function provides estimates of Very Low density Lipoprotein (VLDL) using triglycerides
#'@param VLDL Very Low density Lipoprotein
#'@param triglycerides triglycerides (mgdl)
#'@export
#'

VLDL <- function(triglycerides) {
  # Check if the input value is numeric
  if (!is.numeric(triglycerides)) {
    stop("Input value must be numeric.")
  }

  # Check if the input value is positive
  if (triglycerides <= 0) {
    stop("Input value must be positive.")
  }

  # Calculate the VLDL
  vldl <- triglycerides / 5

  # Return the result
  return(vldl)
}


#Creating simulated data
#my_data <- data.frame(
#height_cm = c(165, 170, 175, 180, 185),
#weight_kg = c(60, 70, 80, 90, 100),
#waist_cm = c(75, 80, 85, 90, 95),
#hip_cm = c(90, 95, 100, 105, 110))

##Trial estimates
#attach(my_data)
#my_data$H=H(height_cm)

#my_data$W=W(weight_kg)

#BMI(W, H)

#detach(my_data)



