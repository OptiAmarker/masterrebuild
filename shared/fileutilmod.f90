module fileutilmod
 use dflib
 integer,parameter:: gmaxfile=112,gwholesmax=34523456,gminfile=11
 integer,parameter:: gmaxlinelen=5000,ilocklistcnt=100,gmemfilemax=5028500
 
 character gmemfile*(gmemfilemax),gmemname*500,gutilret*1000
 ! GEXENAME is in CMDLINEMOD
 !character*300 gexename
 integer igmemloc

 character fileline*(gmaxlinelen),strtemp*(gwholesmax),locklist(100)*500
 integer ifileinitdone
 type fileopentype
  integer size,linecnt,curline,wholeptr,cana,canr,exist,iostat
  character fname*500,openmode
 end type
! I want to have 100 files available. The units
! start at 11 and I want one extra unit, 112,
! to be used for temporary file stats.
 type(fileopentype)gfilestat(gminfile:gmaxfile)

 type wholefiletype
  integer used,pos,curline
  character str*(gwholesmax)
 end type
 type(wholefiletype) wholefile(10)
 
 type(file$info) gfinfoq

 contains

!============================ FILEUTILINIT
  subroutine fileutilinit
  use othermod
  use cmdlinemod
  integer wc(1000)
  real fc(1000)
      real*8 rsubtime,r1,r2
! !DON'T! put a SUBTIMER because FILEUTILINIT is called before MAIN is established.
  call subtimer("fileutilinit",1,rsubtime)
  ifileinitdone=777
  do i=gminfile,gmaxfile
! Set WHOLEPTR explicitly because I don't want any
! weird numbers being used to clear GWHOLSTR.
   gfilestat(i)%wholeptr=-1
   call filefree(i)
  enddo
  do i=1,10
   wholefile(i)%str=" "
   wholefile(i)%used=0
   wholefile(i)%pos=0
   wholefile(i)%curline=0
  enddo
  do i=1,gmaxlinelen; fileline(i:i)=" "; enddo
  
  do i=1,ilocklistcnt; locklist(i)=" "; enddo
  
! Having optimized code is GOOD and this section will test for that.
! I will output a message if the code is NOT optimized. When optimized,
! the calcs below will be much faster. Run them for 0.1 seconds and count
! the iterations obtained that time frame. The calculation
! I use will be 30x faster when optimized (if I can make that multiplier
! bigger, it will be better.
      wc=5
      fc=0.5
      call walltime(r1,1,r2,"")
      ibig=0
      do
       ibig=ibig+1
       isx=sum(wc*fc)
       isx=sum(wc/fc)
       call walltime(r1,2,r2,"")
       if(r2>0.1)exit
      enddo
!      print "(i9,' >= 20000 = Good!')",ibig
      if(ibig<20000)then
       call printheader("Code is NOT optimized!","*",40,0,1,0)
      endif
7     continue
      
      call syslog(":init:")
      
      call initmemfile
      call sysinit
      call avast
  call subtimer("fileutilinit",2,rsubtime)

  return
  end subroutine

!============================ FILEFREE
  subroutine filefree(isl)
  use othermod
      real*8 rsubtime
  data ifirst/1/
  call subtimer("filefree",1,rsubtime)
  
  i=isl
! if this file was opened in memory, clear
! that memory up here.
  if(gfilestat(i)%wholeptr>0)then
   ip=gfilestat(i)%wholeptr
   wholefile(ip)%str=" "
   wholefile(ip)%used=0
   wholefile(ip)%pos=0
   wholefile(ip)%curline=0
  endif
   gfilestat(i)%iostat=0
   gfilestat(i)%size=-1
   gfilestat(i)%linecnt=-1
   gfilestat(i)%curline=-1
   gfilestat(i)%wholeptr=-1
   gfilestat(i)%cana=-1
   gfilestat(i)%canr=-1
   gfilestat(i)%exist=-1
   gfilestat(i)%fname=" "
   gfilestat(i)%openmode=" "
  call subtimer("filefree",2,rsubtime)
  return
  end subroutine

!============================ FCLOSE
  subroutine fclose(iff)
  use othermod
  real*8 rsubtime
  call subtimer("fclose",1,rsubtime)

! If this is a mem file, dump it out  
  if(iff==999)then
   call dumpmemfile(gmemname)
   call initmemfile
   goto 20
  endif

  iu=abs(iff)
! If this is a whole file and I'm not deleting it, skip this
   if(gfilestat(iu)%openmode=="")call enditall("Can't close a file that's not open!")
  if(gfilestat(iu)%wholeptr>0.and.iff>0)then
   goto 10
  elseif(gfilestat(iu)%wholeptr>0.and.iff<0)then
! If I want to delete a whole file, I have to open it again first
   open(gmaxfile,file=gfilestat(iu)%fname)
   close(gmaxfile,status="delete")
   goto 10
  elseif(gfilestat(iu)%wholeptr<=0.and.iff<0)then
   close(iu,status="delete",iostat=istat)
  else
   close(iu)
  endif

10 continue
  call filefree(iu)
20 continue  
  call subtimer("fclose",2,rsubtime)
  return
  end subroutine

!============================ FILESTAT
  subroutine filestat(fl)
  use othermod
  real*8 rsubtime
  character fl*(*)
  logical lex,lop
  call subtimer("filestat",1,rsubtime)
  call fileinitcheck
  isl=gmaxfile
  call filefree(isl)
  gfilestat(isl)%fname=fl
  inquire(file=fl,exist=lex,size=isize,iostat=istat,opened=lop)
  if(istat>0)gfilestat(isl)%iostat=istat
  if(lex)then
   gfilestat(isl)%exist=1
   gfilestat(isl)%size=isize
  else
! if the file doesn't exist, go ahead and flag that is can
! be appended to. Also, give it a size of 0.
   gfilestat(isl)%exist=0
   gfilestat(isl)%cana=1
   gfilestat(isl)%size=0
   goto 20
  endif
  open(unit=isl,file=fl,iostat=istat,action="read")
  if(istat>0)then
   gfilestat(isl)%iostat=istat
   gfilestat(isl)%canr=0
   goto 20
  else
   gfilestat(isl)%canr=1
  endif
  close(isl)
  open(unit=isl,file=fl,iostat=istat,access="append")
  if(istat>0)then
   gfilestat(isl)%iostat=istat
   gfilestat(isl)%cana=0
  else
   gfilestat(isl)%cana=1
  endif
! If file didn't exist, opening it for write will create it with
! zero size, so I need to delete it.
20 continue  
 if(.not.lex)then
   close(isl,status="delete",iostat=istat)
  else
   close(isl)
  endif
  
10 continue
   inquire(file=fl,iostat=istat,opened=lop)
   call subtimer("filestat",2,rsubtime)
   return
  end subroutine

!============================ IFILCHK
  function ifilchk(fl,mode)
  use othermod
  character fl*(*),mode
  real*8 rsubtime
  call subtimer("ifilchk",1,rsubtime)
  if(mode=="r")goto 20
  if(mode=="w")goto 20
  if(mode=="e")goto 20
  call enditall("IFILCHK: Invalid mode: "//mode)
20 continue
  iret=1
  isl=gmaxfile
  call filestat(fl)
  if(mode=="e".and.gfilestat(isl)%exist==0)then
   iret=0
   goto 10
  endif
  if(mode=="r".and.gfilestat(isl)%canr<=0)then
   iret=0
   goto 10
  endif
  if(mode=="w".and.gfilestat(isl)%cana==0)then
   iret=0
   goto 10
  endif
 10 continue
  ifilchk=iret
  call subtimer("ifilchk",2,rsubtime)
  return
  end function

!============================ IFOPEN
  function ifopen(fl,mode)
! MODE - "r" - read
!        "R" - read, but file HAS to be opened into memory as a wholefile
!        "b" - binary, NOT eligible for WHOLEFILE because there's no way of
!              knowing how big the file will get if I write to it.
!        "B" - binary, but file HAS to exist already
!        "w" - write new file
!        "a" - append to file
!        "m" - This is an output file, but I'm keeping the contents in memory
  use othermod
  character fl*(*),mode,tmpmode
  real*8 rsubtime
  
   call subtimer("ifopen",1,rsubtime)
   if(mode=="m")then
    if(len_trim(gmemname)>0)then
     call enditall("I'm opening a MEM file ("//trim(fl)//"), but haven't written the previous one ("//trim(gmemname)//")!")
    endif
    ifopen=999
! Delete the file here. It will make debugging easier because if the file
! is missing, then I KNOW I'm using memory.
    call delfile(fl,1)
    call initmemfile
    gmemname=fl
    goto 60
   endif
  
  tmpmode=mode
  if(mode=="R")tmpmode="r"
  if(mode=="B")tmpmode="b"
  call filestat(fl)
  isl=ifreeunit()

! Copy info from the temp area to the main area
  gfilestat(isl)=gfilestat(gmaxfile)
  gfilestat(isl)%openmode=mode

! Find out if HOW I'm opening the file works with
! the file stats. For instance, I can't append to
! a file that's read-only. I can't read a file that
! doesn't exist. By the way, these should be
! program-ending errors if I get to them at this point.
  if(gfilestat(isl)%exist==0.and.tmpmode=="r")then
   call enditall("Can't read a missing file! >"//trim(fl)//"<")
  endif
  if(gfilestat(isl)%cana==0.and.(tmpmode=="w".or.tmpmode=="a"))then
   call enditall("Can't write to a read-only file! "//trim(fl))
  endif      
  if(gfilestat(isl)%exist==0.and.mode=="B")then
   call enditall("Can't read a missing binary file! "//trim(fl))
  endif

! If this file can be opened into memory, do that here.
! Only files I'm reading are eligible.
  iw=0
  if(mode=="b")goto 40
  if(gfilestat(isl)%size<=gwholesmax.and.(tmpmode=="r".or.tmpmode=="b"))then
   do i=1,10
    if(wholefile(i)%used==0)then; iws=i; goto 30; endif
   enddo
   goto 40
30 continue    
   iw=1
   open(unit=isl,file=gfilestat(isl)%fname,form="unformatted",access="stream",iostat=istat)
   isize=gfilestat(isl)%size
   gfilestat(isl)%wholeptr=iws
   wholefile(iws)%used=1
   if(isize>0)read(isl,pos=1)wholefile(iws)%str(1:isize)
   close(isl,iostat=istat)
   goto 50
  endif
40 continue
! I'm here if my file is too big for memory, or if I already
! have too many files open in memory and I need to open normally.
! If I NEED this file to be opened in memory, stop the program
! and tell me if it's a size problem.
  if(mode=="R")then
   print "(a)","File failed to load to memory, but is HAS to be loaded there!"
   print "(a,i0)","File size: ",gfilestat(isl)%size
   print "(a,i0)"," Mem size: ",gwholesmax
   print "(a,a)","File name: ",trim(fl)
   if(gwholesmax>=gfilestat(isl)%size)print "(a)","It's not a size problem, there aren't any whole slots left!"
   call enditall("")
  endif
  select case(tmpmode)
  case("r")
   open(unit=isl,file=gfilestat(isl)%fname,action="read",iostat=istat)
  case("w")
   open(unit=isl,file=gfilestat(isl)%fname,action="write",iostat=istat,status='replace')
! put a size of zero on new files so I know the slot is taken
   gfilestat(isl)%size=0
  case("a")
   open(unit=isl,file=gfilestat(isl)%fname,access="append",iostat=istat)
  case("b")
   open(unit=isl,file=gfilestat(isl)%fname,form="unformatted",access="stream",iostat=istat)
  case default
   call enditall("Invalid open type: "//mode)
  end select
  gfilestat(isl)%iostat=istat
50 continue 
  ifopen=isl
  
  if(tmpmode=="a".or.tmpmode=="o")then
   gfilestat(isl)%linecnt=0
   goto 60
  endif
 ! get line count
  ilc=0
  do
   call nextline(isl,ieof)
   if(ieof/=0)exit
   ilc=ilc+1
!   call progtot("lines: ",ilc,iov,0)
  enddo
  gfilestat(isl)%linecnt=ilc
  gfilestat(isl)%curline=0
  call rewindf(isl)
60 continue  
  call subtimer("ifopen",2,rsubtime)
  return
  end function

!========================================= NEXTLINE
  subroutine nextline(iff,ieof)
  use othermod
  character fmt*10
  real*8 rsubtime
!  integer zone(10)
   call subtimer("nextline",1,rsubtime)
  ieof=0
  if(gfilestat(iff)%wholeptr>0)then
   iw=gfilestat(iff)%wholeptr
   ip=wholefile(iw)%pos
   ifs=gfilestat(iff)%size
! this is a debug thing that will fill ZONE with the
! integer values of the bytes around POS
!   call initari(zone,10,-1)
!   do i=1,10
!    ii=ip-4+i
!    if(ii<1)cycle
!    if(ii>gfilestat(iff)%size)cycle
!    zone(i)=ichar(wholefile(iw)%str(ii:ii))+(ii*10000)
!    if(ii==ip)zone(i)=-1*zone(i)
!   enddo
! find the next end-of-line, x0A (ascii 10)
   ip=ip+1
! if I'm searching past EOF, then set EOF to 1
   if(ip>gfilestat(iff)%size)then
    fileline=":EOF:"
    ieof=1
    goto 10
   endif
! if I didn't find another x0A, then my file doesn't end with a
! line return. Set the end point as the end of the whole string.
! I used to search the ENTIRE rest of the string, but this took a LONG time for
! files that didn't end with a carriage return. Instead, I just search up to the
! file size, which is MUCH faster!
   ii=index(wholefile(iw)%str(ip:ifs),achar(10))
   if(ii==0)then
!    THis line is not a good idea in the case that my file ends
!    in spaces, but no x0A
!    ii=len_trim(wholefile(iw)%str)+1
     ii=gfilestat(iff)%size+1
   else
    ii=ii+ip-1
   endif
! got it, but I don't want to include it in the string
! if an x0D was before that, don't include it either.
! It's possible to have a x0A as the very first character in the
! file. If ii=1, then that has happened and I know I don't have
! anything before it.
   ib=1
   if(ii==1)goto 20
   i55=ichar(wholefile(iw)%str(ii-1:ii-1))
   if(i55==13)ib=2
20 continue   
   fileline=wholefile(iw)%str(ip:ii-ib)
! set my pointer to be sitting on the x0A
   wholefile(iw)%pos=ii
   wholefile(iw)%curline=wholefile(iw)%curline+1
   gfilestat(iff)%curline=gfilestat(iff)%curline+1
10 continue 
  else
   iii=gmaxlinelen
   write(fmt,"('a',i0)"),iii
   read(iff,"("//trim(fmt)//")",iostat=ieof),fileline
   gfilestat(iff)%curline=gfilestat(iff)%curline+1
  endif
   call subtimer("nextline",2,rsubtime)
  return
  end subroutine

!================================ GETSTRING
  subroutine getstring(iff,ist,il,str)
  use othermod
  character str*(*)
  real*8 rsubtime
   call subtimer("getstring",1,rsubtime)
  do i=1,len(str); str(i:i)=" "; enddo
  if(gfilestat(iff)%wholeptr>0)then
   iw=gfilestat(iff)%wholeptr
   ix=ist+il-1
   if(ix>gwholesmax)ix=gwholesmax
   str=wholefile(iw)%str(ist:ix)
  else
   read(iff,pos=ist,iostat=istat),str(:il)
   ill=len_trim(str)
!   do i=1,ill
!    print *,ichar(str(i:i))
!   enddo
  endif
   call subtimer("getstring",2,rsubtime)
  return
  end subroutine

!================================ PUTSTRING
  subroutine putstring(iff,ist,il,str)
  use othermod
  character str*(*)
  real*8 rsubtime
!  do i=1,len(str); str(i:i)=" "; enddo
   call subtimer("putstring",1,rsubtime)
  if(gfilestat(iff)%wholeptr>0)then
   iw=gfilestat(iff)%wholeptr
   ix=ist+il-1
   if(ix>gwholesmax)ix=gwholesmax
   wholefile(iw)%str(ist:ix)=str
  else
   write(iff,pos=ist,iostat=istat),str(:il)
  endif
   call subtimer("putstring",2,rsubtime)
  return
  end subroutine

!================================= FILEINITCHECK
  subroutine fileinitcheck
  use othermod  
  if(ifileinitdone/=777)call enditall("FILEUTILINIT must be called first!")
  return
  end subroutine

!================================= DELFILE
  subroutine delfile(fl,imakesurein)
! IMAKESURE - 0 - Assume this file will be deleted with no issues.
!             1 - Make sure the file is gone. Stop if not.
!            <0 - Wait for a maximum of ABS(IMAKESURE) seconds for
!                 the file to be deleted. Stop if not.
! After careful consideration, I've decided to make option 1 the same as
! passing IMAKESUREIN as -5. Anything important enough to stop the
! program should probably be given a little grace period.

  use othermod
  character fl*(*)
  logical lex,lop
  real*8 rsubtime,rs,re
  
   call subtimer("delfile",1,rsubtime)
  isec=0
  if(imakesurein==1)then
   isec=5
   imakesure=0
   goto 10
  elseif(imakesurein<0)then
   isec=abs(imakesurein)
   imakesure=0
   goto 10
  else
   imakesure=abs(imakesurein)
  endif
  
!  print *,gmaxfile,ifreeunit()
  open(gmaxfile,file=fl,iostat=istato)
  close(gmaxfile,status="delete",iostat=istatc)
  if(imakesure==1)then
   inquire(file=fl,exist=lex)
   if(lex)call enditall(trim(fl)//" deleted but still exists!!")
  endif

! Try for up to X seconds to delete the file.
10 continue
  if(isec>0)then
   call walltime(rs,1,re,"")
   do
    inquire(file=fl,exist=lex,opened=lop)
    if(.not.lex)then
!     print "(f7.2,' ',a)",re,trim(fl)
     exit
    endif
    open(gmaxfile,file=fl,iostat=istato,action="write")
    close(gmaxfile,status="delete",iostat=istatc)
    call walltime(rs,2,re,"")
    if(re>real(isec))call enditalli("File not deleted after X seconds: "//trim(fl)//" - ",isec)
    call stalltime(0.1)
   enddo
  endif
   call subtimer("delfile",2,rsubtime)
  
  return
  end subroutine

!================================ LOCKFILE
  subroutine lockfile(fl,imode,iflg)
  use othermod
  use cmdlinemod
  real*8 rsubtime
! IMODE 0 - wait here until file is unlocked, then lock it and continue
!       1 - if the file is unlocked, lock it and continue
!           if the file is locked, set IFLG to 1 and report back but don't wait
  character fl*(*),str*500,d*500
  logical lex
   call subtimer("lockfile",1,rsubtime)
! In order to lock a file, the directory has to exist.
  ix1=index(fl,"/",.TRUE.)
  ix2=index(fl,"\",.TRUE.)
  if(ix1+ix2>0)then
   if(ix2>ix1)ix1=ix2
   d=fl(:ix1)
!  if this directory doesn't exist, I can't lock the file.
   if(idirchk(d)==0)then
    call enditall("Can't lock file in missing directory: "//trim(d))
   endif
  endif
  str=trim(fl)//".locked"
  ifirst=1
  iflg=0
! If IGZEROLOCK is set, I don't care if there is a lock file or not, I 
! just want to blaze on through. In fact, remove any lock file I DO see.
  if(igzerolock==1)then
   call delfile(str,1)
   goto 10
  endif
30 continue
  open(gmaxfile+1,file=str,status="new",err=20,iostat=istat,action="write")
!  print *,"open  ",istat
  write(gmaxfile+1,*,iostat=istat),"This is locked! (Gasp!) Nooooo!"
!  print *,"write ",istat
  if(istat>0)then
   close(gmaxfile+1,status="delete",iostat=istat)
!   print *,"close delete ",istat
   if(istat>0)call enditall("Problem close-deleting file...")
   goto 20
  else
   close(gmaxfile+1)
  endif
  do i=1,ilocklistcnt
   if(len_trim(locklist(i))==0)exit
  enddo
  if(i>ilocklistcnt)call enditall("Too many locked files!")
  locklist(i)=fl
  goto 10
20 continue
  if(imode==1)then; iflg=1; goto 10; endif
  if(ifirst==1)print "(' ',a$)",trim(fl)//" locked. Waiting..."
  ifirst=0
  goto 30
10 continue
  if(ifirst==0)print "(a)"," continuing!"
   call subtimer("lockfile",2,rsubtime)
  return
  end subroutine

!================================ UNLOCKFILE
  subroutine unlockfile(fl)
  use othermod
  real*8 rsubtime
  character fl*(*),str*500
   call subtimer("unlockfile",1,rsubtime)
  str=trim(fl)//".locked"
  call delfile(str,0)
  do i=1,ilocklistcnt
   if(fl==locklist(i))then
    locklist(i)=""
    exit
   endif
  enddo
   call subtimer("unlockfile",2,rsubtime)
  return
  end subroutine

!================================== WRITELINE
      subroutine writeline(chunk,iwhere)
! writes a line to a file IWHERE or the screen if IWHERE=0
! if IWHERE is negative, it goes to both, where the file is ABS(IWHERE)
      use othermod
      character chunk*(*)
  real*8 rsubtime
   call subtimer("writeline",1,rsubtime)
      idebug=isdebugactive("writeline")

      ilwhere=iwhere
      if(iwhere<0)ilwhere=-1*iwhere

! If this is not a memory file...
      if(ilwhere/=999)then
! ... make sure it's a valid range.      
       if(ilwhere>0.and.(ilwhere<11.or.ilwhere>100))then
        print "(a,i0,a)","WRITELINE: Invalid file unit of ",ilwhere,". Must be between 11 and 100."
        call enditall("")
       endif
      endif

      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv  writeline"
       print *,"iwhere: ",iwhere
       print *,"ilwhere: ",ilwhere
       print *,":"//trim(chunk)//":"
      endif
      if(iwhere<=0)then
       print "(a)",trim(chunk)
      endif
      if(iwhere>0.or.ilwhere>0)then
       if(ilwhere==999)then
        call writememfile(chunk)
       else
        write (ilwhere,"(a)",iostat=istat),trim(chunk)
       endif
!       print "(a)",trim(chunk)
      endif
      if(idebug==1)then
       print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  writeline"
      endif
   call subtimer("writeline",2,rsubtime)
      return
      end subroutine

!======================================== DIRLIST
      subroutine dirlist(sdir,imakesure,ipath,iok)
! IMAKESURE 0 - if directory doesn't exist, it's okay.
!           1 - directory HAS to exist, or else stop the program!
! IPATH     0 - just print file name to DIRLIST.DLS
!           1 - prefix the file with the path name
      use othermod
      character sdir*(*),str*1000
  real*8 rsubtime
   call subtimer("dirlist",1,rsubtime)
      idebug=isdebugactive("dirlist")
      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvvv   dirlist"
       print *,"directory: ",trim(sdir)
      endif
      call delfile("dirlist.dls",1)
      call delfile("dirlist.err",1)
      if(ipath==0)then
       call system(trim(gsysloc)//"ls.exe -1ap "//trim(sdir)//" >dirlist.dls 2>dirlist.err")
      else
       call system(trim(gsysloc)//"ls.exe -1apd "//trim(sdir)//"* >dirlist.dls 2>dirlist.err")
      endif
      if(ifilchk("dirlist.err","r")==0)then
       ifs=0
       goto 20
      else
       ifd=ifopen("dirlist.err","r")
       ifs=gfilestat(ifd)%size
      endif
      iok=1
      if(ifs>0)then
       iok=0
! the directory listing failed, but if I don't care, skip error.
       if(imakesure==0)goto 10
       print *,"Directory listing for "//trim(sdir)//" failed. Here's the error file..."
       print *,""
       print *,"v v v v v v v v v v v v v v v v v v v v"
       print *,""
       do
        call nextline(ifd,ieof)
        if(ieof/=0)exit
        print *,trim(fileline)
       enddo
       print *,""
       print *,"^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^"
       call fclose(ifd)
       call delfile("dirlist.dls",1)
       call enditall("")
 10    continue
      endif
      call fclose(-ifd)
20    continue
      if(idebug==1)print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   dirlist"
   call subtimer("dirlist",2,rsubtime)
      return
      end subroutine

!========================================= FILECOPY
      subroutine filecopy(fl1,fl2,iwarn)
! IWARN 1 - if FL1 doesn't exist, STOP the program!
!       0 - if FL1 doesn't exist, do nothing and continue
      use othermod
      character fl1*(*),fl2*(*)
  real*8 rsubtime

   call subtimer("filecopy",1,rsubtime)
      idebug=isdebugactive("filecopy")
      if(idebug==1)then
       call debhed(1,"filecopy")
       print *,"source: ",trim(fl1)
       print *,"  dest: ",trim(fl2)
      endif
      
! if this file doesn't exist, but I don't care, continue
      if(ifilchk(fl1,"r")==0.and.iwarn==0)goto 10

      ifi=ifopen(fl1,"R")
      if(gfilestat(ifi)%wholeptr<=0)call enditall("Can't open file like needed for copy!!!")
      iff=ifopen(fl2,"w")
      write(iff,"(a$)"),wholefile(gfilestat(ifi)%wholeptr)%str(1:gfilestat(ifi)%size)
      call fclose(iff)
      call fclose(ifi)
10    continue
      if(idebug==1)call debhed(0,"filecopy")
   call subtimer("filecopy",2,rsubtime)

      return
      end subroutine

!========================================= FILEMOVE
      subroutine filemove(fl1,fl2)
      use othermod
      character fl1*(*),fl2*(*)
  real*8 rsubtime
   call subtimer("filemove",1,rsubtime)
      call filecopy(fl1,fl2,1)
      call delfile(fl1,1)
   call subtimer("filemove",2,rsubtime)

      return
      end subroutine
!=================================== WHOLEPOS
      subroutine wholepos(iff,ipos)
      use othermod
  real*8 rsubtime
! this sets a pointer for a file that I have opened the
! WHOLE thing in memory. This could get nasty for CURLINE.
! If IPOS is zero, just set CURLINE to zero. Any other value,
! and I need to figure out what line I'm on.
   call subtimer("wholepos",1,rsubtime)
      ip=gfilestat(iff)%wholeptr
      wholefile(ip)%pos=ipos
      if(ipos==0)then
       wholefile(ip)%curline=0
       gfilestat(iff)%curline=0
      endif
   call subtimer("wholepos",2,rsubtime)
      return
      end subroutine
!================================== IFILEFIND
      function ifilefind(str)
      use othermod
      real*8 rsubtime
      character str*(*)
   call subtimer("ifilefind",1,rsubtime)
      iret=-1
      do i=gminfile,gmaxfile
       if(gfilestat(i)%fname==str)then
        iret=i
        goto 10
       endif
      enddo
      call enditall("File not open! "//trim(str))
10    continue 
      ifilefind=iret
   call subtimer("ifilefind",2,rsubtime)
      return
      end function
!============================== IFREEUNIT
  function ifreeunit()
  use othermod
  real*8 rsubtime
! find a free slot
   call subtimer("ifreeunit",1,rsubtime)
  do i=gminfile,gmaxfile
   if(gfilestat(i)%size==-1)goto 20
  enddo
  call enditall("Too many files open!")
 20 continue
  ifreeunit=i
   call subtimer("ifreeunit",2,rsubtime)
  return
  end function
  
!========================================= FILEAPPEND
      subroutine fileappend(fl1,fl2,idel)
      use othermod
! Appends FL1 to the bottom of FL2. FL1 is then deleted if IDEL=1
      character fl1*(*),fl2*(*)
      real*8 rsubtime

   call subtimer("fileappend",1,rsubtime)
      if(ifilchk(fl1,"r")==0)goto 99
      ifi=ifopen(fl1,"r")
      ifo=ifopen(fl2,"a")
      do
       call nextline(ifi,ieof)
       if(ieof/=0)exit
       call writeline(fileline,ifo)
      enddo
      call fclose(ifi)
      call fclose(ifo)
      if(idel==1)call delfile(fl1,1)
99    continue       
   call subtimer("fileappend",2,rsubtime)

      return
      end subroutine

!========================================= IWHOLESLOT
      function iwholeslot(ifl)
      use othermod
      real*8 rsubtime
   call subtimer("iwholeslot",1,rsubtime)
      iret=0
      if(ifl<gminfile.or.ifl>gmaxfile)then
       call enditalli("Bad File Ask: ",ifl)
      endif
      iret=gfilestat(ifl)%wholeptr
      iwholeslot=iret
   call subtimer("iwholeslot",2,rsubtime)
      return
      end function
      
!===================================== UPDATEBWHOLE
      subroutine updatebwhole(iff)
      use othermod
! If I open a file in B mode, it could be opened as a whole
! file. If I make changes to it, I need to update the file
! that it came from. Do that here.
      character fname*100
      logical lex
      real*8 rsubtime
      
   call subtimer("updatebwhole",1,rsubtime)
      if(gfilestat(iff)%openmode/="B")goto 10
      iw=iwholeslot(iff)
      if(iw<=0)goto 10
      isz=gfilestat(iff)%size
      strtemp=wholefile(iw)%str
      fname=gfilestat(iff)%fname
      call fclose(iff)
      ifn=ifopen(fname,"w")
      write(ifn,"(a$)",iostat=istat),strtemp(1:isz)
      call fclose(ifn)
      iff=ifopen(fname,"B")
10    continue 
   call subtimer("updatebwhole",2,rsubtime)
      return
      end subroutine

!=============================== MD5SUM
      subroutine md5sum(fl,md5)
      use othermod
      character md5*(*),fl*(*),exe*200
      real*8 rsubtime
   call subtimer("md5sum",1,rsubtime)
      if(len(md5)/=32)call enditalli("Bad MD5 size: ",len(md5))
      if(ifilchk(fl,"r")/=1)then
       md5="?"
       goto 20
      endif
      exe="c:/cygwin/bin/md5sum.exe"
      if(ifilchk(exe,"r")==1)goto 10
      exe="c:/andydos/utils/md5.exe"
      if(ifilchk(exe,"r")==1)goto 10
      call enditall("Can't find MD5!!!")
10    continue      
      call system(trim(exe)//' "'//trim(fl)//'" > md5.tmp')
      iff=ifopen("md5.tmp","r")
      call nextline(iff,ieof)
      call fclose(-iff)
      md5=fileline(1:32)
20    continue 
   call subtimer("md5sum",2,rsubtime)
      return
      end subroutine

!================================= REWINDF
      subroutine rewindf(ifi)
      use othermod
      character fn*500,om
      real*8 rsubtime
   call subtimer("rewindf",1,rsubtime)
      iw=iwholeslot(ifi)
      if(iw<=0)then
       rewind(ifi,iostat=istat)
       if(istat/=0)call enditalli("Problem with REWIND! ",istat)
       gfilestat(ifi)%curline=0
      else
       call wholepos(ifi,0)
      endif
   call subtimer("rewindf",2,rsubtime)
      return
      end subroutine
      
!=========================== ECHOFILE
      subroutine echofile(str,mode,fil)
      use othermod
      character str*(*),mode,fil*(*),lm
      real*8 rsubtime
      call subtimer("echofile",1,rsubtime)
      lm=mode
      if(mode=="o")then; lm="w"; goto 10; endif
      if(mode=="w")goto 10
      if(mode=="a")goto 10
      call enditall("Invalid mode: "//mode)
10    continue
      iff=ifopen(fil,lm)
      write(iff,"(a)"),trim(str)
      call fclose(iff)
      call subtimer("echofile",2,rsubtime)
      return
      end subroutine
      
!============================ CLEANLOCKS
      subroutine cleanlocks
      use othermod
      real*8 rsubtime
      call subtimer("cleanlocks",1,rsubtime)
      do i=1,ilocklistcnt
       if(len_trim(locklist(i))>0)call unlockfile(locklist(i))
      enddo
      call subtimer("cleanlocks",2,rsubtime)
      return
      end subroutine
      
!============================ IDIRCHK
      function idirchk(str)
      use othermod
      character str*(*)
      real*8 rsubtime
      call subtimer("idirchk",1,rsubtime)
      iret=0
! This next line works on Windows, but not unix so I now create
! a file in the directory I'm checking and test for that file.
!      inquire(directory=str,exist=lex)
      ift=ifopen(trim(str)//"/0dircheck.123","w")
! If I have a filecode, but IOSTAT is not zero, I have no file,
! probably because this directory doesn't exist.
      if(ift>10)then
       if(gfilestat(ift)%iostat/=0)then
        call fclose(ift)
        goto 10
       endif
      endif
      write(ift,"(a)",iostat=istat),"?"
      call fclose(ift)
      ix=ifilchk(trim(str)//"/0dircheck.123","r")
      call delfile(trim(str)//"/0dircheck.123",1)
      if(ix==1)iret=1
10    continue
      idirchk=iret
      call subtimer("idirchk",2,rsubtime)
      return
      end function
      
!====================== FILEPATH
      subroutine filepath(fl,loclist,ilistlen,islot)
      use othermod
      character fl*(*),loclist(10)*500
      real*8 rsubtime
! Look for a file in a series of locations. Return the index in LOCLIST
! of the first location I find it at.
      call subtimer("filepath",1,rsubtime)
      islot=0
      do i=1,ilistlen
       if(ifilchk(trim(loclist(i))//fl,"r")==1)then
        islot=i
        exit
       endif
      enddo
      if(islot==0)then
       print "(a)",""
       do i=1,ilistlen
        print "(a)",trim(loclist(i))
       enddo
      endif
      call subtimer("filepath",2,rsubtime)
      return
      end subroutine

!============================= MR
      subroutine mr(ifi,icnt)
      use othermod
      real*8 rsubtime
! read multiple lines from file
      call subtimer("mr",1,rsubtime)
      do i=1,icnt
       call nextline(ifi,ieof)
      enddo
      call subtimer("mr",2,rsubtime)
      return
      end subroutine
      
!===================== SYSLOG
      subroutine syslog(cmd)
      use othermod
      use cmdlinemod
! this routine will execute CMD, but will also log
! the command to a log file.
      character cmd*(*)
      real*8 rsubtime
      
      call subtimer("syslog",1,rsubtime)
      if(cmd==":init:")then
       call delfile("syscmd.log",1)
      else
       if(igmultiuser==0)call echofile(cmd,"a","syscmd.log")
       if(ignoexe==0)call system(cmd)
      endif
      call subtimer("syslog",2,rsubtime)
      return
      end subroutine
      
!==================== STOPPEEK
      subroutine stoppeek(istop)
      use othermod
      istop=0
      if(ifilchk("stopit.txt","r")==1)istop=1
      return
      end subroutine
      
!==================== STOPCHECK
      subroutine stopcheck(idel)
      use othermod
      real*8 rsubtime
      call subtimer("stopcheck",1,rsubtime)
      if(ifilchk("stopit.txt","r")==1)then
       if(idel==1)call delfile("stopit.txt",1)
       call enditall("STOPIT.TXT found, so stopping.")
      endif
      call subtimer("stopcheck",2,rsubtime)
      return
      end subroutine
       
!=================== INITMEMFILE
      subroutine initmemfile
      use othermod
      igmemloc=0
      gmemfile=""
      gmemname=""
      return
      end subroutine
      
!================== DUMPMEMFILE
      subroutine dumpmemfile(fl)
      use othermod
      character fl*(*)
      real*8 rsubtime
      call subtimer("dumpmemfile",1,rsubtime)
      ifi=ifopen(fl,"w")
      write(ifi,"(a)"),gmemfile(:igmemloc)
      call fclose(ifi)
      call subtimer("dumpmemfile",2,rsubtime)
      return
      end subroutine
      
!================= WRITEMEMFILE
      subroutine writememfile(str)
      use othermod
      character str*(*)
      real*8 rsubtime
      call subtimer("writememfile",1,rsubtime)
      il=len_trim(str)
      if(il+igmemloc+2>gmemfilemax)call enditall("GMEMFILE is too big!")
      if(igmemloc==0)then
       gmemfile(:il)=str
       igmemloc=il
      else
       i=igmemloc+1
       gmemfile(i:i)=achar(13)
       i=i+1
       gmemfile(i:i)=achar(10)
       i=i+1
       gmemfile(i:i+il-1)=str
       igmemloc=igmemloc+il+2
      endif
      call subtimer("writememfile",2,rsubtime)
      return
      end subroutine
      
!============================= AUTOTEXT
!      subroutine autotext(fl,ifi,ieof)
!!  This works like NEXTLINE, but it will open the file for the first read
!!  and close the file after EOF is found.
!      character fl*(*)
!      
!      if(ifi<11.or.ifi>gmaxfile-1)ifi=ifopen(fl,"r")
!      ieof=0
!      call nextline(ifi,ieof)
!      if(ieof/=0)then; call fclose(ifi); ifi=0; endif
!      return
!      end subroutine

!============================ IFILESAME
      function ifilesame(fl1,fl2,iret)
! Return a 0 if files are the same.
! Return a value of >0 depending on what difference a file has.
! 1 - size
! 2 - string compare
! 3 - md5sum didn't match
! 4 - at least one of the files is missing
      use othermod
! Based on MD5 checksum, are two files the same?
      character m1*32,m2*32,fl1*(*),fl2*(*)
      logical le1,le2
      real*8 rsubtime
      call subtimer("ifilesame",1,rsubtime)
      ifilesame=0
      iret=0
! Since MD5SUM takes a while to run, I first do an inquiry as to
! each file's size. If different, so are they.
      inquire(file=fl1,size=is1,exist=le1)
      inquire(file=fl2,size=is2,exist=le2)
      if(.not.le1.or..not.le2)then;iret=4;goto 10;endif
      if(is1/=is2)then;iret=1;goto 10;endif
! If this file is too big to load to memory, just do an MD5SUM
      if(is1>gmemfilemax)goto 30
! MD5SUM should be a last resort because it takes awhile to run.
! Load both files. Check compare the first half of each file to find
! any differences. I load several characters at once to make it fast.
      if1=ifopen(fl1,"B")
      if2=ifopen(fl2,"B")
      iw1=gfilestat(if1)%wholeptr
      iw2=gfilestat(if2)%wholeptr
      ilc=5000
      ifb=is1
! start at file position 1
      ipos=1
      idf=0
      do
! set the upper bound of where I will read
       iub=ipos+ilc-1
! if the upper bound is greater than my file boundary,
! read characters to file boundary.
       if(iub>ifb)iub=ifb
! get my strings
       if(wholefile(iw1)%str(ipos:iub)/=wholefile(iw2)%str(ipos:iub))then
        idf=1
        exit
       endif
! set my new start spot. If it's greater than the file
! boundary, I'm all done!
       ipos=ipos+ilc
       if(ipos>ifb)exit
      enddo
      call fclose(if1)
      call fclose(if2)
      if(idf==1)then;iret=2;goto 10;endif
      ifilesame=1
      goto 10
30    continue
      call md5sum(fl1,m1)
      call md5sum(fl2,m2)
      if(m1/=m2)then;iret=3;goto 10;endif
      ifilesame=1
10    continue
      
      call subtimer("ifilesame",2,rsubtime)
      return
      end function
      
!=========================== LOADINI
      subroutine loadini(keyin,val,imust)
! IMUST 0 - KEYIN doesn't have to exist.
!       1 - If KEYIN not found, stop program!
! Look in [progname].ini for settings. The start of the line must be KEYIN with
! any character after it, the whatever's after that character is the setting. Examples...
!FILEPATH=c:/datafiles/
!USERNAME:bigmclargehuge
      use cmdlinemod
      use strutilmod
      use othermod
      character keyin*(*),key*(len(keyin)),val*(*),ini*1000,fk*(len(keyin))
      
      ix=index(gexename,".",.TRUE.)
      ini=gexename(:ix-1)//".ini"
!      call lockfile(ini,0,iflg)
      if(ifilchk(ini,"r")==1)goto 10
      ini=trim(gprogpath)//gexename(:ix-1)//".ini"
      if(ifilchk(ini,"r")==1)goto 10
      ini=gexename(:ix-1)//".ini"
      call enditall("Can't find "//trim(ini))
10    continue
      call strinit(val)
      val="?"
      key=keyin
      call lcase(key)
      ilk=len_trim(key)
      
      ifi=ifopen(ini,"r")
      do
       call nextline(ifi,ieof)
       if(ieof/=0)exit
       if(len_trim(fileline)==0)cycle
       if(fileline(1:1)=="#")cycle
       fk=fileline
       call lcase(fk)
       if(fk/=key)cycle
       val=fileline(ilk+2:)
       val=adjustl(val)
       exit
      enddo
      call fclose(ifi)
      
      if(val=="?")then
       if(imust==1)call enditall(keyin//" not found in "//trim(ini))
       call initstr(val)
      endif
!      call unlockfile(ini)
      
      return
      end subroutine
      
!============================== MEMFILESCAN
      subroutine memfilescan(iff,str,isucc,ix10)
! If I'm looking for a certain line in a file I have loaded into memory, the current
! way is to read the file line-by-line until I find. But is there an easier way? This
! routine will search the string containing the entire file for STR. If it finds it,
! it will set the WHOLEFILE pointer to point to the beginning of the line.
! IX10 0 - to simulate NEXTLINE logic where I read a line starting with the character
!          after a hex 10 character, find the hex 10 character before my match and
!          put the file pointer there. This is the option to use when you want it to
!          work as normal.
!      1 - Just put the pointer where the match is made. This is a very special case
!          and usually only used when you don't really want to include the matched
!          text in the next NEXTLINE call.
      character str*(*)

      isucc=-1
! Silently do nothing if:
! 1 - This file is not loaded into memory
! 2 - I don't have a search string.
      if(gfilestat(iff)%wholeptr<=0)goto 99
      if(len_trim(str)==0)goto 99
      
      iwptr=gfilestat(iff)%wholeptr
      ix=index(wholefile(iwptr)%str,trim(str))
! If I didn't find a match, do nothing.
      if(ix<=0)goto 99
      
! Search backwards for the x0A. Set the pointer to that position.
      if(ix10==0)then
       iax=index(wholefile(iwptr)%str(:ix),achar(10),.TRUE.)
      else
       iax=ix-1
      endif
      call wholepos(iff,iax)
      isucc=iax
      
! I'm now sitting at the right place, however, since I did a quick
! search, my current line is all messed up. Count up all of the x10's
! that appear before the new position.
      ist=1
      ied=iax
      ilin=0
      do
       ix=index(wholefile(iwptr)%str(ist:ied),achar(10))
       if(ix==0)exit
       ilin=ilin+1
       ist=ist+ix
      enddo
      gfilestat(iff)%curline=ilin
      
      
99    continue
      return
      end subroutine

!==================================== LOCKFILE2
      subroutine lockfile2(fl)
      use othermod
      use cmdlinemod
      character fl*(*),d*500
      logical lop
! In order to lock a file, the directory has to exist.
      ix1=index(fl,"/",.TRUE.)
      ix2=index(fl,"\",.TRUE.)
      if(ix1+ix2>0)then
       if(ix2>ix1)ix1=ix2
       d=fl(:ix1)
!  if this directory doesn't exist, I can't lock the file.
       if(idirchk(d)==0)then
        call enditall("Can't lock file in missing directory: "//trim(d))
       endif
      endif
!  If I'm ignoring locked files, smash any I find
      if(igzerolock==1)then
       call delfile(trim(fl)//".locked",1)
       goto 99
      endif
! Make sure this program doesn't already have this file locked.
! If it does, then pretend that this routine was successful.
! Keep track of the first blank slot I find because I will use that
! slot to lock the file.
      ifb=0
      do i=1,ilocklistcnt
       if(locklist(i)==fl)goto 99
       if(len_trim(locklist(i))==0.and.ifb==0)ifb=i
      enddo
      if(ifb>ilocklistcnt.or.ifb<=0)call enditall("Too many locked files!")
      ifirst=1
      istat=0
10    continue
      if(ifirst==0)print "(a)",trim(fl)//" locked. Waiting..."
      if(ifirst==1)then;ifirst=0;else;ifirst=2;endif
      open(gmaxfile+ifb,file=trim(fl)//".locked",status='new',err=10,action="write")
      write(gmaxfile+ifb,"(a)",iostat=istat),"This is locked! (Gasp!) Nooooo!"
      locklist(ifb)=fl
99    continue
      return
      end subroutine
      
!==================================== UNLOCKFILE2
      subroutine unlockfile2(fl)
      use othermod
      use cmdlinemod
      character fl*(*)
      logical lex,lop

      if(igzerolock==1)goto 99
! If the lock file doesn't exist, nothing to do!
      inquire(file=trim(fl)//".locked",exist=lex,number=iunit,opened=lop)
      if(.not.lex)goto 99
      close(iunit,status="delete",iostat=istat)
      if(istat/=0)call enditall("Problem close-deleting lockfile: "//trim(fl)//".locked")
      locklist(iunit-gmaxfile)=""
99    continue
      return
      end subroutine
      
!==================================== SMASHFILETYPE
      subroutine smashfiletype(ft)
      character*(*) ft
      call dirlist(trim(ft),0,0,iok)
      ifi=ifopen("dirlist.dls","r")
      do
       call nextline(ifi,ieof); if(ieof/=0)exit
       call delfile(fileline,1)
      enddo
      call fclose(ifi)
      iii=1
      return
      end subroutine
      
!============================ FILECOPYB
      subroutine filecopyb(src,dest)
      character bstr*1524000
      character*(*) src,dest
      call delfile(dest,1)
      ifi=ifopen(src,"B")
      ifo=ifopen(dest,"b")
      ix=len(bstr)
      ifl=gfilestat(ifi)%size
      ist=1
      istop=0
      do
       ied=ist+ix-1
       if(ied>ifl)then
        ix=mod(ifl,ix)
        istop=1
       endif
       call getstring(ifi,ist,ix,bstr)
       call putstring(ifo,ist,ix,bstr)
       if(istop==1)exit
       ist=ist+ix
       if(ist>ifl)exit
      enddo
      call fclose(ifi)
      call fclose(ifo)
      return
      end subroutine

!===================== SYSLOGRET
      subroutine syslogret(cmd)
      use othermod
      use cmdlinemod
! This works just like SYSLOG, but it surrounds the command
! with an SH so command can be returned to the caller before
! the CMD is finished processing.
      character cmd*(*)
      character c2*5000
      
      c2=trim(gsysloc)//'sh.exe -c "'//trim(cmd)//' &"'
      call syslog(c2)
      iii=1
      
      return
      end subroutine
      
!===================== SYSINIT
      subroutine sysinit
      use othermod
      gsysloc="c:/cygwin/bin/"
      if(ifilchk(trim(gsysloc)//"sh.exe","r")==1)goto 10
      gsysloc="c:/mksnt/bin/"
      if(ifilchk(trim(gsysloc)//"sh.exe","r")==1)goto 10
      gsysloc="c:/mksnt/mksnt/"
      if(ifilchk(trim(gsysloc)//"sh.exe","r")==1)goto 10
      gsysloc="c:/mksnt/"
      if(ifilchk(trim(gsysloc)//"sh.exe","r")==1)goto 10
      gsysloc="c:/user/amarker/other/fortran/visualstudio/utils/cygbin/"
      if(ifilchk(trim(gsysloc)//"sh.exe","r")==1)goto 10
      call enditall("Can't locate system directory with SH.EXE")
10    continue
      return
      end subroutine

!=========================== FINDUTIL
subroutine findutil(u)
! Check a series of pre-defined places for utilities.
! First, try to be smart and find stuff in this order... 
! - The RUN dir
! - The EXE dir
! - The EXE dir + utils folder
! - The EXE dir , up 1 dir, + utils folder
! - The EXE dir , up 2 dirs, + utils folder
! - The RUN dir + utils folder
! - The RUN dir , up 1 dir, + utils folder
! - The RUN dir , up 2 dirs, + utils folder
!   If none those work, try certain hard-coded directories where
!   I usually keep stuff.

use cmdlinemod
use othermod
character u*(*),p*5000
logical lex
iii=1

gutilret=trim(grunpath)//u; if(ifilchk(gutilret,"r")==1)goto 10
gutilret=trim(gprogpath)//u; if(ifilchk(gutilret,"r")==1)goto 10

! strip a dir level off

! start of path loop
do ipx=1,2
if(ipx==1)p=gprogpath
if(ipx==2)p=grunpath
il=len_trim(p)
if(il==0)goto 15
if(p(il:il)/="/")p(il:il)="/"
gutilret=trim(p)//"utils/"//u; if(ifilchk(gutilret,"r")==1)goto 10
if(p(il:il)=="/")p(il:il)=" "
ix=index(p,"/",.TRUE.)
if(ix==0)goto 15
p=p(:ix)
gutilret=trim(p)//"utils/"//u; if(ifilchk(gutilret,"r")==1)goto 10

! strip another dir level off
il=len_trim(p)
if(il==0)goto 15
if(p(il:il)=="/")p(il:il)=" "
ix=index(p,"/",.TRUE.)
if(ix==0)goto 15
p=p(:ix)
gutilret=trim(p)//"utils/"//u; if(ifilchk(gutilret,"r")==1)goto 10
15 continue
enddo
! end of path loop
gutilret="c:/andydos/utils/"//u; if(ifilchk(gutilret,"r")==1)goto 10
gutilret="c:/user/amarker/utils/"//u; if(ifilchk(gutilret,"r")==1)goto 10
gutilret="c:/windows/system32/"//u; if(ifilchk(gutilret,"r")==1)goto 10
gutilret="c:/user/amarker/utils/wgetdir/"//u; if(ifilchk(gutilret,"r")==1)goto 10
gutilret="c:/user/amarker/other/sheet/PortableApps/WinWGetPortable/App/winwget/wget/"//u; if(ifilchk(gutilret,"r")==1)goto 10
gutilret="c:/windows/system32/"//u; if(ifilchk(gutilret,"r")==1)goto 10

call enditall("Can't locate "//trim(u))
10 continue
print "(a)","Found: "//trim(gutilret)
return
end subroutine

!=============================== FILEENTRY
subroutine fileentry(imode,s,f,iret)
! IMODE 0 - check for S in file F. Return 0 or 1.
!       1 - If S is not already in F, add it.

character*(*) f,s

iret=0; isucc=-1
if(ifilchk(f,"r")==0)goto 10
ifi=ifopen(f,"r")
call memfilescan(ifi,s,isucc,0)
call fclose(ifi)
if(isucc>=0)iret=1
10 continue
if(imode==0)goto 99
if(imode==1.and.isucc>=0)goto 99

ifi=ifopen(f,"a")
write(ifi,"(a)"),trim(s)
call fclose(ifi)

99 continue

return
end subroutine

!=============================== CREATEDIR
      subroutine createdir(dn,imode)
! IMODE 0 - if DN already exists, it's NOT an error
!       1 - if DN already exists, it IS an error
      use dflib
      use othermod
      character dn*(*)
      ih=makedirqq(dn)
      if(ih<=0)then
       ih=getlasterrorqq()
       if(ih==err$exist.and.imode==0)goto 10
       call enditalli("Dir create failed! "//trim(dn),ih)
10     continue
      endif
      return
      end subroutine
      
!========================= CATFILE
      subroutine catfile(f)
      character f*(*)
      ifi=ifopen(f,"r")
      do
       call nextline(ifi,ieof); if(ieof/=0)exit
       print "(a)",trim(fileline)
      enddo
      call fclose(ifi)
      return
      end subroutine
      
!======================== EXITCODE
      subroutine exitcode(str,ic)
      character str*(*)
      print "(a)",trim(str)
      call exit(ic)
      return
      end subroutine
      
!=========================== FILELISTQ
      subroutine filelistq(str,imode,imakesure,ipath,fn,iend)
! Use DFLIB to get file listings. This is faster than LS.
! IMODE - -1 - initialize file listing
!          0 - get the next file name. Set IEND=1 if done.
!         -2 - Do a count of the files and return in IEND
! IMAKESURE - 0 - if directory doesn't exist, simply set iend to 1 and return
!             1 - "                        ", end program immediately!
! IPATH -  0 - just return the naked file name.
!          1 - return the entire path to the file name.
      use dflib
      use othermod
      character*80 files
      character*500,save:: dir
      character*(*) str,fn
      integer,save:: ih
      logical lex
      
      iii=1
      imodein=imode
      
      if(imode==-2)imode=-1
      ifcnt=0
      
      iend=0
      if(imode==-1)then
       dir=" "
       imode=0
       ih=file$first
! Get the directory, if one is provided
       ix1=index(str,"/",.TRUE.)
       ix2=index(str,"\",.TRUE.)
       ix=ix1
       if(ix2>ix)ix=ix2
       if(ix>0)then
        dir=str(:ix)
!       make sure directory exists
        inquire(directory=dir,exist=lex)
        if(.not.lex)then
         if(imakesure==1)then
          call enditall("FILELISTQ: Dir doesn't exist: "//trim(dir))
         else
          iend=1
         endif
        endif
       endif
      endif
      
      if(imode==0)then
10     continue
       il=getfileinfoqq(str, gfinfoq, ih)
       fn=" "
       if(ih<0)then
        iend=1
        if(imodein==-1)inofile=1
       else
! I only want files. Is this actually a directory?
        fn=trim(dir)//gfinfoq%name
        inquire(directory=fn,exist=lex)
! If so, get the next filename
        if(lex)goto 10
! If I only want the filename without path, set it here.
        if(ipath==0)fn=gfinfoq%name
        ifcnt=ifcnt+1
       endif
      endif
      if(imodein==-2)then
       if(iend==0)goto 10
       iend=ifcnt
      endif
99    continue
      return
      end subroutine
      
!=========================== DIRLISTQ
      subroutine dirlistq(str,imode,imakesure,ipath,fn,iend)
! Use DFLIB to get dir listings. This is faster than LS.
! IMODE - -1 - initialize dir listing
!          0 - get the next dir name. Set IEND=1 if done.
! IMAKESURE - 0 - if directory doesn't exist, simply set iend to 1 and return
!             1 - "                        ", end program immediately!
! IPATH -  0 - just return the naked dir name.
!          1 - return the entire path to the dir.
      use dflib
      use othermod
      character*80 files
      character*500,save:: dir,strl
      character*(*) str,fn
      integer,save:: ih
      logical lex
      
      iii=1
      imodein=imode
      
      iend=0
      if(imode==-1)then
       strl=str
       ilen=len_trim(str)
       if(str(ilen:ilen)=="/")strl=trim(str)//"*"
       dir=" "
       imode=0
       ih=file$first
! Get the directory, if one is provided
       ix1=index(strl,"/",.TRUE.)
       ix2=index(strl,"\",.TRUE.)
       ix=ix1
       if(ix2>ix)ix=ix2
       if(ix>0)then
        dir=strl(:ix)
!       make sure directory exists
        inquire(directory=dir,exist=lex)
        if(.not.lex)then
         if(imakesure==1)then
          call enditall("FILELISTQ: Dir doesn't exist: "//trim(dir))
         else
          iend=1
         endif
        endif
       endif
      endif
      
      if(imode==0)then
10     continue
       il=getfileinfoqq(strl, gfinfoq, ih)
       fn=" "
       if(ih<0)then
        iend=1
        if(imodein==-1)inofile=1
       else
! If NAME is this direcotry, or the one above, I don't care. Skip
        if(gfinfoq%name==".".or.gfinfoq%name=="..")goto 10
! I only want dirs. Is this one?
        fn=trim(dir)//gfinfoq%name
        inquire(directory=fn,exist=lex)
! If not, get the next dir
        if(.not.lex)goto 10
! If I only want the filename without path, set it here.
        if(ipath==0)fn=gfinfoq%name
       endif
      endif
      return
      end subroutine
      
!========================== WR
      subroutine wr(s,i)
      character s*(*)
      call writeline(s,i)
      return
      end subroutine
      
!=============================== AVAST
      subroutine avast
! The AVAST virus scanner must scan the exe every time it changes.
! To prevent that, echo the current directory to a file. If this
! is an AVAST run, it will fail and the file will exist, but have zero
! size. If this is the case, stop execution. If the file looks good,
! continue on.
      use othermod
      use strutilmod
      character cmd*500
! Since this has the potential to hault execution, I need to be in
! AVAST mode. Since I run this before GETCMDLINE, I need to manually go
! through the command-line here.
      iaa=0
      icnt=iargc()
      do i=1,icnt
       call getarg(i,cmd); call ucase(cmd)
       if(cmd(:6)=="-AVAST")then
        iaa=1
        exit
       endif
      enddo
      if(iaa==0)goto 99
      call system(trim(gsysloc)//'pwd.exe > avast.txt"')
      inquire(file="avast.txt",exist=lex,size=isize)
      open(file="avast.txt",unit=37)
      close(37,status="delete")
      if(isize==0)then
       print "(a)",""
       print "(a)","This is an AVAST run, and therefore means naught."
       print "(a)",""
       call enditall("")
      endif
99    continue
      return
      end subroutine

      
end module
