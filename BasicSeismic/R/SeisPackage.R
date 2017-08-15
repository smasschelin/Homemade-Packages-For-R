
#This equation gives the P-Wave velocity for a homogenous, isotropic medium (km/s). BulkMod referes to the Bulk Modulus, ShearMod the Shear Modulus, and Rho the density of the medium (kg/m3)
PVelocity <- function(BulkMod, ShearMod, Rho) {
  sqrt((BulkMod + (4/3*ShearMod))/Rho)
}
#This equation gives the S-Wave Velocity for a homogenous, isotropic medium (km/s). ShearMod refers to the Shear Modulus, Rho refers to the density of the medium (kg/m3).
SVelocity <- function(ShearMod, Rho){
  sqrt(ShearMod/Rho)
}
#This equation gives the generalized Poisson Ratio equation for the length change (i.e. uniaxial extension)  for an homogenous, isotropic material. DeltaL is extension, and DeltaLPrime represents shortening. Note that L is dimensionless, but you must be consistent in the units used.

PoiRatioLen <- function(DeltaL, DeltaLPrime) {
  -(DeltaLPrime/DeltaL)
}
#This equation gives the generalized Poisson Ratio equation for change in volume for an homogenous material. DeltaL and DeltaLPrime remain the same as in PoiRatioLen
PoiRationVol <- function(DeltaL, DeltaLPrime, DeltaV, DeltaVPrime) {
    0.5 - ((L*DeltaVPrime)/(2*DeltaL*DeltaV))
}
#This equation gives the Bulk Modulus for a material.  Rho is the density in kg/m3, and dPdRho is the derivative of pressure with respect to density.
BulkModEq <- function(Rho, dPdRho) {
    rho * dPdRho
}
#An alternate version of the Bulk Modulus equation. Volume is, naturally, the volume in m3, and dPdV is the derivative of pressure with respect to volume.
BulkModEq1 <- function(Volume, dPdV)
    -Volume * dPdV

#This equation gives the Young's Modulus of an homogenous, isotropic material. This is the force over an area version rather than the tensile/extensional stress version. Force is, naturally, the force applied in N, OrigLength is the original lenght of the material, FCrossArea is the cross-sectional area over which the force is applied (m2), and PureDeltaL is the amount by which the length of the object changes.
YoungMod <- function(Force, OrigLength, FCrossArea, PureDeltaL){
    (Force*OrigLength)/(FCrossArea*PureDeltaL)
}


