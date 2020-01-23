! DAXPY program     Y=A*X+Y where X and Y are vectors    Jan 23  2020
!
! 
      program DAXPY_Test 
      !Define Variables 
      real,parameter::A=2              !constant multiplier 
      integer :: i,Xsize,Ysize
      integer,parameter::n=5
      real,dimension(n)::X,Y,Yinv    !square Matricies 
      
      Xsize=size(X)
      Ysize=size(Y)
     

      !Populate Matrix Z,Y 
      do i=1,Xsize 
        X(i)=i
        Y(i)=2*i
      end do 
    
      !Print Matrix to check
      print*,"X Matrix is:",X
      print*,"Y Matrix is:",Y 

      !Function Call 
      call DAXPY(A,X,Y,Xsize)
      !Print the Output from DAXPY function
      print*,"DAXPY Output Matrix Y:",Y

      end program DAXPY_Test 


      subroutine  DAXPY(A,X,Y,Xsize)
      real,intent(in)::X(*)
      real,intent(inout)::Y(*)
      integer,intent(in)::Xsize
      real,intent(in)::A

        if (A>0) then
          !Scalar Multiplication then addition
          do i=1, Xsize
             Y(i)=A*X(i)+Y(i)
          end do
        end if
        return 
      end subroutine DAXPY

