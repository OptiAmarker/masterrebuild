module cmdlinemod
 integer igspeed,igquiet,igerror,iqperf
 character*300 gexename,gprogpath,grunpath*300
 contains
!=============== GETCMDLINE =============
      subroutine getcmdline
      use strutilmod
      character*30 cmd
      igspeed=0
      igquiet=0
      igerror=1
      iqperf=2000
      iargcnt=iargc()
      do i=1,iargcnt
       call getarg(i,cmd)
       call ucase(cmd)
       if(cmd=="-STOP")exit
       if(cmd=="-SPEED")then
        call getarg(i+1,cmd)
        read(cmd,*),igspeed
       endif
       if(cmd=="-QUIET")igquiet=1
       if(cmd=="-NOERROR")igerror=0
       if(cmd=="-QPERF")then
        call getarg(i+1,cmd)
        read(cmd,*),iqperf
       endif
      enddo
      return
      end subroutine
end module