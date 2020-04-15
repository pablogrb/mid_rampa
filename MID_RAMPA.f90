PROGRAM MID_RAMPA

USE MID_RAMPA_MODELS
USE class_UAM_IV
IMPLICIT NONE

    ! ------------------------------------------------------------------------------------------
    ! Purpose:
    ! 	Calculates aerodynamically resuspended (wind blown) emissions using wind dependant
    !   emission factors
    ! Inputs
    ! 	3d meteorology file in CAMx 6.0+ format
    ! 	Surface area file (with silt loading)
    ! Outputs
    ! 	Area source emission file
    ! By:
    ! 	Pablo Garcia
    ! 	pablogrb@gmail.com
    ! 	UPB - GIA
    ! 	2020-04
    ! This program requires a F08 compatible compiler
    ! ------------------------------------------------------------------------------------------
    ! Error coodes
    !	0 = File IO
    !	1 = Memory allocation
    !   2 = Unsupported feature

    ! ------------------------------------------------------------------------------------------
    ! Declarations

    ! IO
    CHARACTER(LEN=256) :: met_imp                   ! Input UAM-IV CAMx 3D Meteorology file path
    CHARACTER(LEN=256) :: sas_imp                   ! Surface area file (with silt loading)
    INTEGER :: sas_imp_unit
    CHARACTER(LEN=256) :: emis_out                  ! CAMx6.20 compatible area file with wind-blown dust emissions

    ! UAM_IV files
    TYPE(UAM_IV) :: fl_met                          ! Input UAM-IV CAMx 3D Meteorology file

    ! Wind speed
    REAL, ALLOCATABLE :: wind_array(:,:,:)           ! Wind velocity magnitude (windspeed) (col, row, hour)
    INTEGER :: uwind_isp, vwind_isp                 ! Species code of wind components

    ! Surface area and silt loading
    REAL, ALLOCATABLE :: sas_array(:,:,:)           ! Surface area and silt (SAS) loading array (col, row, (area, silt))
    INTEGER :: sas_x, sas_y                         ! x and y cell coordinate of SAS data
    REAL :: sas_area, sas_silt                      ! SAS data for validation before entering into the output file array

    ! Emissions model
    CHARACTER(LEN=10) :: model

    ! Control
	INTEGER :: arg_num
	CHARACTER(LEN=2) :: arg_switch
    LOGICAL :: file_exists
    INTEGER :: alloc_stat
    INTEGER :: io_stat
    INTEGER :: i_sas

    ! Namelist IO
	CHARACTER(LEN=256) :: ctrlfile					! Control namelist
    INTEGER :: nml_unit								! Control file unit
    NAMELIST /file_io/ met_imp, sas_imp, emis_out
    NAMELIST /emis_model/ model

    ! ------------------------------------------------------------------------------------------
	! Entry point
	! ------------------------------------------------------------------------------------------

	! Command line argument capture
	arg_num = COMMAND_ARGUMENT_COUNT()
	IF (arg_num .EQ. 0) THEN
		ctrlfile = 'MID_RAMPA.nml'
	ELSEIF (arg_num .NE. 2) THEN
		WRITE(0,'(A)') 'Bad argument number'
		CALL EXIT(0)
	ELSE
	! 	Capture the argument type
		CALL GET_COMMAND_ARGUMENT(1,arg_switch)
		IF (arg_switch .NE. '-f') THEN
			WRITE(0,'(A)') 'Bad argument type'
			CALL EXIT(0)
		ELSE
			CALL GET_COMMAND_ARGUMENT(2,ctrlfile)
		END IF
	END IF
	! Check if the file exists
	INQUIRE(FILE=TRIM(ctrlfile), EXIST=file_exists)
	IF (file_exists) THEN
		WRITE(6,'(2A)') 'Using the control file ', TRIM(ctrlfile)
	ELSE
		WRITE(0,'(3A)') 'Control file ', TRIM(ctrlfile), ' does not exist'
		CALL EXIT(0)
    END IF
    
    ! Read the namelist
	OPEN(NEWUNIT=nml_unit, FILE=ctrlfile, FORM='FORMATTED', STATUS='OLD', ACTION='READ')
    READ(nml_unit,NML=file_io)
    READ(nml_unit,NML=emis_model)
    CLOSE(nml_unit)
    
    ! ------------------------------------------------------------------------------------------
    ! Check if the met file exists
	INQUIRE(FILE=TRIM(met_imp), EXIST=file_exists)
	IF (.NOT. file_exists) THEN
		WRITE(0,'(A)') 'Point source parameter file ', TRIM(met_imp), ' does not exist'
		CALL EXIT(0)
    END IF
    
    ! Read the met input file
    CALL inquire_header(fl_met, met_imp)
    ! Check for file type
    IF ( fl_met%ftype .NE. 'AVERAGE   ' ) THEN
        WRITE(*,'(A)') 'Bad file type. File must be of type AVERAGE'
        CALL EXIT(0)
    END IF
    ! Check if wind component variables are available
    uwind_isp = fl_spindex(fl_met,'UWIND_MpS')
    vwind_isp = fl_spindex(fl_met,'VWIND_MpS')
    ! WRITE(*,*) 'U component is in position ', uwind_isp, ' and V in ', vwind_isp
    
    ! Allocate memory to the windspeed array
    ALLOCATE(wind_array(fl_met%nx,fl_met%ny,fl_met%update_times), STAT=alloc_stat)
    CALL check_alloc_stat(alloc_stat)

    SELECT CASE (fl_met%nzup)
    CASE (1)
        WRITE(0,'(A)') 'Horizontal wind components are in an Arakawa C arrangement'
        WRITE(0,'(A)') 'This arrangement is currently not supported'
        CALL EXIT(2)

    CASE (0)
        WRITE(*,'(A)') 'Horizontal wind components are in an cell center arrangement'
        
        ! Calculate the wind velocity magnitude field (windspeed)
        ! windspeed = fl_met%conc(:,:,1,:,uwind_isp)**2
        wind_array = SQRT(fl_met%conc(:,:,1,:,uwind_isp)**2 + fl_met%conc(:,:,1,:,vwind_isp)**2)
        ! WRITE(*,'(A)') 'Windspeed calculation worked'

    CASE DEFAULT
        WRITE(0,'(A)') 'Bad data in wind staggering flag'
        CALL EXIT(0)

    END SELECT

    ! ------------------------------------------------------------------------------------------
    ! Check if the surface area file exists
	INQUIRE(FILE=TRIM(sas_imp), EXIST=file_exists)
	IF (.NOT. file_exists) THEN
		WRITE(0,'(A)') 'Point source parameter file ', TRIM(sas_imp), ' does not exist'
		CALL EXIT(0)
    END IF

    ! Allocate the SAS array
    ALLOCATE(sas_array(fl_met%nx, fl_met%ny, 2), STAT=alloc_stat)
    CALL check_alloc_stat(alloc_stat)
    sas_array = 0.

    ! Read the surface area parameter file
	OPEN(NEWUNIT=sas_imp_unit, FILE=TRIM(sas_imp),STATUS='OLD')
	! Skip column headers
    READ(sas_imp_unit,*)
    
    ! Read the SAS file until error or EOF
    i_sas = 0
    DO WHILE ( io_stat .EQ. 0 )

        READ(sas_imp_unit,*,IOSTAT=io_stat) sas_x, sas_y, sas_area, sas_silt

        ! Validate the x,y data
        IF ( sas_x <= 0 .OR. sas_x > fl_met%nx .OR. sas_y <= 0 .OR. sas_y > fl_met%ny ) THEN
            WRITE(0,'(A,I3,A,I3)') 'y cell coordinate ', sas_x, ' or y cell coordinate ', sas_y, ' is out of the grid bounds'
            CALL EXIT(0)
        END IF
        ! Validate the area and silt data
        IF ( sas_area < 0 .OR. sas_silt < 0. ) THEN
            WRITE(0,'(A)') 'Surface area and silt loading data values cannot be negative'
            CALL EXIT(0)
        END IF

        ! Load the current data point into the array
        ! area
        sas_array(sas_x, sas_y, 1) = sas_area
        sas_array(sas_x, sas_y, 2) = sas_silt

        i_sas = i_sas +1

    END DO

    ! Check if ended on error
    IF ( io_stat > 0 ) THEN
        WRITE(0,'(A)') 'Error reading surface area and silt loading file'
        CALL EXIT(0)
    END IF
    WRITE(*,'(A,I3,A)') 'Read ', i_sas, ' surface area and silt loading records'


END PROGRAM MID_RAMPA

!	------------------------------------------------------------------------------------------
!	Subroutines and functions
!	------------------------------------------------------------------------------------------

SUBROUTINE check_alloc_stat(alloc_stat)

	INTEGER, INTENT(IN) :: alloc_stat

	IF ( alloc_stat .NE. 0 ) THEN
		WRITE(0,'(A)') 'Error allocating parameter vectors, check available memory'
		CALL EXIT(1)
	END IF

END SUBROUTINE check_alloc_stat