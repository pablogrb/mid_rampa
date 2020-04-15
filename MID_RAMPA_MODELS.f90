!	------------------------------------------------------------------------------------------
!	MID_RAMPA Emission models
!	------------------------------------------------------------------------------------------

MODULE MID_RAMPA_MODELS
IMPLICIT NONE

    ! Inputs to models
    ! REAL :: windspeed   ! For field wind tunnel models
    ! REAL :: silt_load   ! For silt loading based models, both the windspeed and silt loading
                        !   are required (lab tunnels)

    ! Model output
    ! All models return an emissions aray: emis(FCRS, CCRS)
    !   where FCRS is Fine Crustal (diameter ≤ 2.5 μm)
    !     and CCRS is Coarse Crustal (2.5 μm < diameter ≤ 10 μm)

CONTAINS


! ------------------------------------------------------------------------------------------
! Silt loading models

! MA11_HADT: Martucevicius et al. 2011. City center street (High traffic).
PURE FUNCTION MA11_HADT(windspeed, silt_load) RESULT(emis)
IMPLICIT NONE

    ! IO
    REAL, INTENT(IN) :: windspeed
    REAL, INTENT(IN) :: silt_load
    REAL :: emis(2)
    ! Model parameters
    REAL :: MA11_HADT_PM25_A
    REAL :: MA11_HADT_PM25_B
    REAL :: MA11_HADT_PM10_A
    REAL :: MA11_HADT_PM10_B

    ! Parameter values
    ! Separated from declaration to mantain PURE condition
    MA11_HADT_PM25_A =  0.133425485257599
    MA11_HADT_PM25_B = -0.251661588317556
    MA11_HADT_PM10_A =  1.854407817103230
    MA11_HADT_PM10_B = -3.500296437526020

    ! FCRS
    emis(1) = silt_load * (MA11_HADT_PM25_A * LOG(windspeed) + MA11_HADT_PM25_B)
    ! CCRS
    emis(2) = silt_load * (MA11_HADT_PM10_A * LOG(windspeed) + MA11_HADT_PM10_B)

END FUNCTION MA11_HADT

! MA11_HADT: Martucevicius et al. 2011. City center street (High traffic).
PURE FUNCTION MA11_LADT(windspeed, silt_load) RESULT(emis)
IMPLICIT NONE

    ! IO
    REAL, INTENT(IN) :: windspeed
    REAL, INTENT(IN) :: silt_load
    REAL :: emis(2)
    ! Model parameters
    REAL :: MA11_LADT_PM25_A
    REAL :: MA11_LADT_PM25_B
    REAL :: MA11_LADT_PM10_A
    REAL :: MA11_LADT_PM10_B

    ! Parameter values
    ! Separated from declaration to mantain PURE condition
    MA11_LADT_PM25_A =  0.0659762214766852
    MA11_LADT_PM25_B = -0.1314029073474510
    MA11_LADT_PM10_A =  2.0119695001067500
    MA11_LADT_PM10_B = -3.9231950934619500

    ! FCRS
    emis(1) = silt_load * (MA11_LADT_PM25_A * LOG(windspeed) + MA11_LADT_PM25_B)
    ! CCRS
    emis(2) = silt_load * (MA11_LADT_PM10_A * LOG(windspeed) + MA11_LADT_PM10_B)

END FUNCTION MA11_LADT

END MODULE MID_RAMPA_MODELS

! References
! Martuzevicius, D., Kliucininkas, L., Prasauskas, T., Krugly, E., Kauneliene, V., Strandberg, B., 2011. 
! 		Resuspension of particulate matter and PAHs from street dust. Atmospheric Environment 45, 310–317. 
!		doi:10.1016/j.atmosenv.2010.10.026