! DAXPY program     Y=A*X+Y where X and Y are vectors    Jan 23  2020
!
!
      real function DAXPY(A,X,Y,Xsize) 
        if (A>0) then 
          !Scalar Multiplication 
          do i=1, Xsize 
             X(i)=A*X(i) 
          end do 

          !Matrix Addition 
          do i=1, Xsize 
             Y(i)=X(i)+Y(i)
          end do 
        end if 
        DAXPY=Y 
      end function DAXPY

      program DAXPY_1 
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
      print*,"DAXPY Output Matrix Y:",DAXPY(A,X,Y,Xsize)

      end program DAXPY_1 

