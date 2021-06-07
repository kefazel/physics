PROGRAM FTCS_heat
!	Purpose :
!	This program solves heat equation by forward time centered space (FTCS) method
!	and gives the results as a [Spatial x Time] size array
! 	Record of revisions:
!		Date		Programmer		Description of change
!		====		==========		=====================
!		06/04/21	V.Sarp			Original code
!
IMPLICIT NONE
! Data dictionary: declare constant types, definitions, & units
REAL, PARAMETER :: PC = 0.14            ! Physical constant of FTCS method
INTEGER, PARAMETER :: S = 12           ! Spatial domain size (meter)
INTEGER, PARAMETER :: T = 100   		! Temporal domain size (second)
REAL, PARAMETER :: dS = .9		    ! Spatial domain differential unit (meter)
REAL, PARAMETER :: dT = .7  		    ! Temporal domain differential unit (second)
INTEGER, PARAMETER :: M = FLOOR(S/dS)   ! Resulting array spatial dimension 
INTEGER, PARAMETER :: N = FLOOR(T/dT)   ! Resulting array temporal dimension 
REAL, DIMENSION(M) :: RESULT 			! Current Resulting array (t -> t)
REAL, DIMENSION(M) :: PRE_RESULT 	    ! Previous Resulting array (t -> t-1)
!
!Data dictionary: declare variable types, definitions, & units
INTEGER :: i 							! loop constant
INTEGER :: j 							! loop constant

WRITE (*,1000) M, N
1000 FORMAT ("Resulting array will include", I3, " columns and ", I3, " rows")

! initialize the first boundary condition: the initial heat of the rod
boundary: DO i = -(M/2), (M/2)-1
			RESULT(i+(M/2)+1) = (M/2)**2 - i**2	
END DO boundary


WRITE (*,1010) RESULT
1010 FORMAT ("Initial heat of the rod is          ", *(F8.2))

! iterate the ftcs equation. print the time step and current heat 
temporal: DO i = 2,N-1

	PRE_RESULT = RESULT

	spatial: DO j=2,M-1
	RESULT(1) = 0
	RESULT(M) = 0
	RESULT(j) = (1.0-2*PC)*PRE_RESULT(j) + PC*(PRE_RESULT(j+1) + PRE_RESULT(j-1))
	END DO spatial

WRITE (*,1020) i, RESULT
1020 FORMAT ("Time Step: ", I3, ".  Heat of the rod is ", *(F8.2))

END DO temporal

END PROGRAM FTCS_heat