PROGRAM MID_RAMPA

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
    ! INTEGER :: sas_imp_unit
    CHARACTER(LEN=256) :: emis_out                  ! CAMx6.20 compatible area file with wind-blown dust emissions

    ! UAM_IV files
    TYPE(UAM_IV) :: fl_met                          ! Input UAM-IV CAMx 3D Meteorology file

    ! Control
	INTEGER :: arg_num
	CHARACTER(LEN=2) :: arg_switch
	LOGICAL :: file_exists

    ! Namelist IO
	CHARACTER(LEN=256) :: ctrlfile					! Control namelist
    INTEGER :: nml_unit								! Control file unit
    NAMELIST /file_io/ met_imp, sas_imp, emis_out

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
    CLOSE(nml_unit)
    
    ! ------------------------------------------------------------------------------------------
    ! Read the met input file
    CALL inquire_header(fl_met, met_imp)
    SELECT CASE (fl_met%nzup)
    CASE (1)
        WRITE(*,'(A)') 'Horizontal wind components are in an Arakawa C arrangement'
        WRITE(*,'(A)') 'This arrangement is currently not supported'
        CALL EXIT(2)
    CASE (0)
        WRITE(*,'(A)') 'Horizontal wind components are in an cell center arrangement'
    CASE DEFAULT
        WRITE(*,'(A)') 'Bad data in wind staggering flag'
        CALL EXIT(0)
    END SELECT


END PROGRAM MID_RAMPA