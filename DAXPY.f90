! DAXPY program     Y=A*X+Y where X and Y are vectors    Jan 23  2020
!
! 
      program DAXPY_Test
      implicit none 
 
      !Define Variables 
      real,parameter::A=2              !constant multiplier 
      integer :: i,Xsize,j
      integer,parameter::n=4
      real,dimension(n,n)::X,Y,Yinv    !square Matricies 
      
      Xsize=size(X,1)
       
      !Populate Matrix X,Y 
      do i=1,Xsize
         do j=1,Xsize 
            X(i,j)=i
            Y(i,j)=j
         end do 
      end do 
    
      !Print Matrix to check
      print*,"X Matrix is:",X
      print*,"Y Matrix is:",Y 

      !Function Call 
      call DAXPY(A,X,Y,Xsize)
      !Print the Output from DAXPY function
      print*,"DAXPY Output Matrix Y:",Y

      !Inverse new matrix using LAPACK
      !call dggetrf(Xsize, ,Yinv,ipiv,info)
      !call dgetri(n,n,Yinv,n, )
           !dgetri parameters: N,A,LDA,IPIV,Work,Lwork,info

      end program DAXPY_Test 


      subroutine  DAXPY(A,X,Y,Xsize)
      implicit none 
      real,intent(in)::X(Xsize,Xsize)
      real,intent(inout)::Y(Xsize,Xsize)
      real,intent(in)::A
      integer,intent(in)::Xsize
      integer::i,j

        if (A>0) then 
          do i=1,Xsize
          do j=1,Xsize
          !Scalar Multiplication then addition
          Y(j,i)=A*X(j,i)+Y(j,i)
          end do 
          end do 
        end if 
      end subroutine DAXPY

