!Test to print matrix 
program Test
real::A(4) 
integer::i, Asize

Asize=size(A)

do i=1,Asize 
  A(i)=i
end do 

print*,A

end program Test   
