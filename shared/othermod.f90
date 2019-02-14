module othermod

 integer,parameter:: imaxsubcnt=500,imaxqsort=5000
 integer idebugcnt,idebuglev(100),igdebstk,igmultiuser
 character debuglist(100)*50,gproglab*100
 character gsysloc*500
 
 real,parameter:: gpi=3.1415926535897932384626433
 
 real rqsv1(imaxqsort),rqsv2(imaxqsort),rqsv3(imaxqsort),rqsv4(imaxqsort),rqsv5(imaxqsort)
 integer iqsv1(imaxqsort),iqsv2(imaxqsort)
 integer iqshowmany,iqsidx(imaxqsort),iqsway1,iqsway2,iqsway3,iqsway4,iqstxttb
 character tqsv1(imaxqsort)*100
 
! Since other modules can't depened on each other in a circular fashion,
! I need to place variables here that I set in CMDLINE, but are used in
! OTHERMOD so everything will compile. In other words, there should NOT
! BE ANY "USE" STATEMENTS IN THIS MODULE!
 integer ignoprog,igsubtime
 
 type subtimetype
  character name*100
  integer cnt,icurmode
  real time,ravg,nottime
 end type
 type(subtimetype)subtime(imaxsubcnt),subtimetmp

 contains

!====================================== LOADDEBUG
  subroutine loaddebug
  character deb*100
  idebugcnt=0
  do i=1,100; debuglist(i)=" "; idebuglev(i)=0; enddo
  
  open(11,file="debug.txt",status="old",iostat=istat,err=10)
  do
   read(11,"(a100)",iostat=istat),deb
   if(istat/=0)exit
   if(deb(1:1)=="#")cycle
   ie=index(deb,"=")
   idl=1
   if(ie>0)then
    read(deb(ie+1:),"(i1)"),idl
    deb=deb(:ie-1)
   endif
   idebugcnt=idebugcnt+1
   debuglist(idebugcnt)=deb
   idebuglev(idebugcnt)=idl
  enddo
  close(11)
10 continue  
  return
  end subroutine

!================================ ISDEBUGACTIVE
  function isdebugactive(str)
  character str*(*)

  idebug=0

  if(idebug==1)print *,"idebugcnt:",idebugcnt

  iret=0
  do i=1,idebugcnt
   if(idebug==1)print *,">"//trim(debuglist(i))//"<"
   if(trim(debuglist(i))==trim(str))then
    iret=1
    if(idebuglev(i)>0)iret=idebuglev(i)
    exit
   endif
   if(trim(debuglist(i))=="debugitall")then; iret=1; exit; endif
  enddo
  isdebugactive=iret

  return
  end function

!======================================== DEBHED
  subroutine debhed(imode,str)
  character str*(*),loc*500,ch,ss*10
  idebug=isdebugactive("debhed")
  if(idebug==1)then
   print *,"%v%v%v%v%v%v%v%v%v%v%v%v%v%v   debhed"
   print "(' ',i0,2a)",imode,"  ",trim(str)
  endif
  iinc=9
  if(imode==1)then; igdebstk=igdebstk+1; ch="v"; endif
  if(imode==0)ch="^"
  loc=""
  if(igdebstk>10)call enditalli("igdebstk is HUGE! ",igdebstk)
  if(igdebstk<1)call enditall("igdebstk is zero!")
  do i=1,(igdebstk*iinc)+6
   loc=trim(loc)//ch
  enddo
!  ss=num2stri(igdebstk)
  write(ss,"(i0)"),igdebstk
  loc=trim(loc)//"   "//trim(str)//"  "//trim(ss)
  print "(a)",trim(loc)
  if(igdebstk==0)call enditall("DEBHED: Stack is zero!!!!")
  if(imode==0)igdebstk=igdebstk-1
  if(idebug==1)then
   print *,"%^%^%^%^%^%^%^%^%^%^%^%^%^%^   debhed"
  endif
  return
  end subroutine

!====================================== ENDITALL
  subroutine enditall(str)
  character str*(*)
  real*8 rdum
  data ienditstack/0/
! keep track of how many times I've called ENDITALL so
! it and SUBTIMER don't call each other in a loop.
  ienditstack=ienditstack+1
  if(len_trim(str)>0)then
   print "(a)",""
   print "(a)",trim(str)
   print "(a)",""
  endif
  if(ienditstack==1)call subtimer("main",2,rdum)
  ienditstack=ienditstack-1
  stop
  return
  end subroutine
!========================= ENDITALLI
  subroutine enditalli(str,i)
  character str*(*),str2*50
!  str2=num2stri(i)
  write(str2,"(i0)"),i
  call enditall(str//" "//str2)
  return
  end subroutine
!========================= ENDITALLII
  subroutine enditallii(str,i,i2)
  character str*(*),str1*50,str2*50
!  str2=num2stri(i)
  write(str1,"(i0)"),i
  write(str2,"(i0)"),i2

  call enditall(str//" "//trim(str1)//"  "//trim(str2))
  return
  end subroutine
!========================= ENDITALLIII
  subroutine enditalliii(str,i,i2,i3)
  character str*(*),str1*50,str2*50,str3*50
!  str2=num2stri(i)
  write(str1,"(i0)"),i
  write(str2,"(i0)"),i2
  write(str3,"(i0)"),i3

  call enditall(str//" "//trim(str1)//"  "//trim(str2)//"  "//trim(str3))
  return
  end subroutine


!============ ISSAMEI =============
  function issamei(ival1,ival2,itol)
  issamei=0
  if(abs(ival1-ival2).le.itol)issamei=1
  return
  end function

!============ ISSAME =============
  function issame(val1,val2,tol)
  real val1,val2,tol
  issame=0
  if(abs(val1-val2).le.tol)issame=1
  return
  end function

!==================== SORTI
      subroutine sorti(ival,iwayin,idx,icnt)
      integer idx(icnt)
      integer ival(icnt)

      idebug=isdebugactive("sorti")

      iway=iwayin
      iext=0
! I can specify an "extra" sort to be done if all values match. The
! extra sort tiebreaker is the actual index value. If IWAY is 12, then
! I sort IVAL ascending and IDX descending if IVAL is equal
      if(iway>9)then
       iway=iwayin/10
       iext=mod(iwayin,10)
      endif

      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv   sorti"
       print *,"sortway: ",iway,"    iext: ",iext
       print *,""
       do i=1,icnt
        print "(i4,a,i11)",i," - ",ival(i)
       enddo
      endif

      call freshidx(idx,icnt)

      do i1=1,icnt-1
       do i2=i1+1,icnt
        ia=idx(i1); ib=idx(i2)
        if(iway==1.and.ival(ia)>ival(ib))goto 10
        if(iway==2.and.ival(ia)<ival(ib))goto 10
        if(iext>0)then
         if(iext==1.and.ia>ib)goto 10
         if(iext==2.and.ia<ib)goto 10
        endif
        goto 20
 10     itmp=idx(i1); idx(i1)=idx(i2); idx(i2)=itmp
 20     continue
       enddo
      enddo
      if(idebug==1)then
       print *,""
       do i=1,icnt
        ia=idx(i)
        print "(i4,a,i11,a,i5)",ia," - ",ival(ia),"     ",i
       enddo
       print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   sorti"
      endif

      return
      end subroutine

!==================== SORTII
      subroutine sortii(ival1,iwayin1,ival2,iwayin2,idx,icnt)
      integer idx(icnt)
      integer ival1(icnt),ival2(icnt)

      idebug=isdebugactive("sortii")

      iway1=iwayin1
      iway2=iwayin2
      iext=0
! I can specify an "extra" sort to be done if all values match. The
! extra sort tiebreaker is the actual index value. If IWAY is 12, then
! I sort IVAL ascending and IDX descending if IVAL is equal
      if(iway2>9)then
       iway2=iwayin2/10
       iext=mod(iwayin2,10)
      endif

      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv   sortii"
       print *,"sortway: ",iwayin1,iwayin2,"    iext: ",iext
       print *,""
       do i=1,icnt
        print "(i4,a,2i11)",i," - ",ival1(i),ival2(i)
       enddo
      endif

      call freshidx(idx,icnt)

      do i1=1,icnt-1
       do i2=i1+1,icnt
        ia=idx(i1); ib=idx(i2)
        if(iway1==1)then
         if(ival1(ia)>ival1(ib))goto 10
         if(ival1(ia)<ival1(ib))goto 20
        endif
        if(iway1==2)then
         if(ival1(ia)<ival1(ib))goto 10
         if(ival1(ia)>ival1(ib))goto 20
        endif

        if(iway2==1)then
         if(ival2(ia)>ival2(ib))goto 10
         if(ival2(ia)<ival2(ib))goto 20
        endif
        if(iway2==2)then
         if(ival2(ia)<ival2(ib))goto 10
         if(ival2(ia)>ival2(ib))goto 20
        endif

        if(iext>0)then
         if(iext==1.and.ia>ib)goto 10
         if(iext==2.and.ia<ib)goto 10
        endif
        goto 20
 10     itmp=idx(i1); idx(i1)=idx(i2); idx(i2)=itmp
 20     continue
       enddo
      enddo
      if(idebug==1)then
       print *,""
       do i=1,icnt
        ia=idx(i)
        print "(i4,a,2i11,a,i5)",ia," - ",ival1(ia),ival2(ia),"     ",i
       enddo
       print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   sortii"
      endif

      return
      end subroutine

!==================== SORTRI
      subroutine sortri(rval1,iwayin1,ival2,iwayin2,idx,icnt)
      integer idx(icnt)
      real rval1(icnt)
      integer ival2(icnt)

      idebug=isdebugactive("sortri")

      iway1=iwayin1
      iway2=iwayin2
      iext=0
! I can specify an "extra" sort to be done if all values match. The
! extra sort tiebreaker is the actual index value. If IWAY is 12, then
! I sort IVAL ascending and IDX descending if IVAL is equal
      if(iway2>9)then
       iway2=iwayin2/10
       iext=mod(iwayin2,10)
      endif

      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv   sortri"
       print *,"sortway: ",iwayin1,iwayin2,"    iext: ",iext
       print *,""
       do i=1,icnt
        print "(i4,a,f11.4,i11)",i," - ",rval1(i),ival2(i)
       enddo
      endif

      call freshidx(idx,icnt)

      do i1=1,icnt-1
       do i2=i1+1,icnt
        ia=idx(i1); ib=idx(i2)
        if(iway1==1)then
         if(rval1(ia)>rval1(ib))goto 10
         if(rval1(ia)<rval1(ib))goto 20
        endif
        if(iway1==2)then
         if(rval1(ia)<rval1(ib))goto 10
         if(rval1(ia)>rval1(ib))goto 20
        endif

        if(iway2==1)then
         if(ival2(ia)>ival2(ib))goto 10
         if(ival2(ia)<ival2(ib))goto 20
        endif
        if(iway2==2)then
         if(ival2(ia)<ival2(ib))goto 10
         if(ival2(ia)>ival2(ib))goto 20
        endif

        if(iext>0)then
         if(iext==1.and.ia>ib)goto 10
         if(iext==2.and.ia<ib)goto 10
        endif
        goto 20
 10     itmp=idx(i1); idx(i1)=idx(i2); idx(i2)=itmp
 20     continue
       enddo
      enddo
      if(idebug==1)then
       print *,""
       do i=1,icnt
        ia=idx(i)
        print "(i4,a,f11.4,i11,a,i5)",ia," - ",rval1(ia),ival2(ia),"     ",i
       enddo
       print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   sortri"
      endif

      return
      end subroutine
      
!============== FRESHIDX
      subroutine freshidx(idx,isiz)
      integer idx(isiz)
! this routine assumes the array starts at sub 1. Any array defind
! differently will be WRONG!!!!!!!
      idebug=isdebugactive("freshidx")

      do i=1,isiz
       idx(i)=i
       if(idebug==1)print *,idx(i),i
      enddo
      return
      end subroutine

!=================================== RDIVI
      function rdivi(i1,i2)
      if(i2==0)then
       rdivi=0.0
      else
       r1=real(i1)
       r2=real(i2)
       rdivi=r1/r2
      endif
      return
      end function

!=================================== RDIV
      function rdiv(r1,r2)
      if(issame(r2,0.0,.0001)==1)then
       rdiv=0.0
      else
       rdiv=r1/r2
      endif
      return
      end function

!=================================== IVRELATION
      function ivrelation(r1,r2,rtol)
! return -1 if r1>r2
!         0 if r1==r2 within a tolerance
!         1 if r1<r2
      idebug=isdebugactive("ivrelation")
      if(idebug==1)then
       call debhed(1,"ivrelation")
       print *,"r1 r2 rtol: ",r1,r2,rtol
      endif
      if(issame(r1,r2,rtol)==1)then; iret=0; goto 10; endif
      if(r1>r2)iret=-1
      if(r1<r2)iret=1
 10   ivrelation=iret
      if(idebug==1)then
       print *,"iret: ",iret
       call debhed(0,"ivrelation")
      endif
      return
      end function

!============ INITARR ===========
      subroutine initarr(rar,icnt,rval)
      real rar(*)
      do i=1,icnt
       rar(i)=rval
      enddo
      return
      end subroutine

!============ INITARR2 ===========
      subroutine initarr2(rar,icnt,icnt2,rval)
      integer icnt,icnt2
      real rar(icnt,icnt2)
      do i=1,icnt
       do ii=1,icnt2
        rar(i,ii)=rval
       enddo
      enddo
      return
      end subroutine

!============ INITARI3 ===========
      subroutine initari3(iar,icnt,icnt2,icnt3,ival)
      integer icnt,icnt2,icnt3
      integer iar(icnt,icnt2,icnt3)
      do i=1,icnt
       do ii=1,icnt2
        do iii=1,icnt3
         iar(i,ii,iii)=ival
        enddo
       enddo
      enddo
      return
      end subroutine

!============ INITARI2 ===========
      subroutine initari2(iar,icnt,icnt2,ival)
      integer icnt,icnt2
      integer iar(icnt,icnt2)
      do i=1,icnt
       do ii=1,icnt2
        iar(i,ii)=ival
       enddo
      enddo
      return
      end subroutine

!============ INITARI ===========
      subroutine initari(iar,icnt,ival)
      integer iar(*)
      real*8 rt
      call subtimer("initari",1,rt)
!      iar(1:icnt)=ival
      do i=1,icnt
       iar(i)=ival
      enddo
      call subtimer("initari",2,rt)
      return
      end subroutine

!============= TIMER
      function timer()
!C returns the time elapsed since this program started
!      call cpu_time(timer)
      call enditall("TIMER is old. Use WALLTIME instead.")
      timer=-999.9
      return
      end function

!=============  SWAPI
      subroutine swapi(i1,i2)
      i=i1
      i1=i2
      i2=i
      return
      end subroutine

!============= SWAPR
      subroutine swapr(r1,r2)
      r=r1
      r1=r2
      r2=r
      return
      end subroutine

!==================== SWAPSTR
      subroutine swapstr(s1,s2)
      character sl*5000,s1*(*),s2*(*)
      sl=s1; s1=s2; s2=sl
      return
      end subroutine

!=========================== STDDEVR
      subroutine stddevr(valu,sval,icnt,rmean)
      real*8 sx,sx2
      real valu(icnt)

      idebug=isdebugactive("stddevr")
      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvv    stddevr"
       do i=1,icnt
        print *,i," - ",valu(i)
       enddo
      endif

      rtot=0; sx=0; sx2=0
      do i=1,icnt
       sx=sx+valu(i)
       sx2=sx2+(valu(i)*valu(i))
       rtot=rtot+valu(i)
      enddo
      if(idebug==1)print *,"rtot:",rtot
      rmean=rtot/icnt
      sval=sqrt(((icnt * sx2) - (sx * sx)) / (icnt * (icnt - 0)))
      if(idebug==1)print *,"^^^^^^^^^^^^^^^^^^^^^^^^^    stddevr"
      return
      end subroutine

!=========================== STDDEVI
      subroutine stddevi(ivalu,sval,icnt,rmean)
      integer*8 i81,i82,i83,i84,isx2,isx,itot
      integer ivalu(icnt)

      idebug=isdebugactive("stddevr")
      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvv    stddevr"
       do i=1,icnt
        print *,i," - ",ivalu(i)
       enddo
      endif

      itot=0; isx=0; isx2=0
      do i=1,icnt
       isx=isx+ivalu(i)
       i81=ivalu(i)
       i81=i81*i81
       i82=isx2
       isx2=isx2+i81
       itot=itot+ivalu(i)
      enddo
      if(idebug==1)print *,"itot:",itot
      rmean=itot/real(icnt)
      if(icnt==0)rmean=0.0
      i81=icnt*isx2
      i82=isx*isx
      i83=icnt
      i83=i83*(i83-0)
      i84=i81-i82
      rv=i84/real(i83)
      if(i83==0)rv=0.0
      sval=sqrt(rv)
      if(idebug==1)print *,"^^^^^^^^^^^^^^^^^^^^^^^^^    stddevr"
      return
      end subroutine

!=============================== PRINTHEADER
      subroutine printheader(str,c,ilin,ib,ia,iff)
      character str*(*),c,s(100)*100
! this routine prints out a header that is one row of C, IL length long
! the next row is STR surrounded by C
! the final row is C, IL length long
! IB and IA are how many blank lines before and after the header
! IFF is the file I'm writing to. 0 if it's the screen

! If IL is negative, then I DON'T print the borders above and below the
! line with STR, but I do still pad based on IB and IA.

      ibord=1
      if(ilin<0)ibord=0
      il=abs(ilin)

      ips=0; ipf=0
      if(iff<=0)ips=1
      if(iff/=0)ipf=abs(iff)

      ils=len_trim(str)
      ib1=(il-(ils+4))/2.0
! first blank lines      
      ic=0
      do i=1,ib
       ic=ic+1; s(ic)=""
      enddo
! first full border
      if(ibord==0)goto 10
      ic=ic+1
      s(ic)=""
      do i=1,il
       s(ic)(i:i)=c
      enddo
10    continue 
! label line
      ic=ic+1
      s(ic)=""
      do i=1,ib1
       s(ic)(i:i)=c
      enddo
      s(ic)=trim(s(ic))//"  "//str
      do i=1,il-ib1-ils-4
       s(ic)(i+ib1+ils+4:i+ib1+ils+4)=c
      enddo
! last full border
      if(ibord==0)goto 20
      ic=ic+1
      s(ic)=""
      do i=1,il
       s(ic)(i:i)=c
      enddo
20    continue 
! last blank lines      
      do i=1,ia
       ic=ic+1; s(ic)=""
      enddo
      do i=1,ic
       if(ips==1)print "(a)",trim(s(i))
       if(ipf>0)write(ipf,"(a)"),trim(s(i))
      enddo
      return
      end subroutine

!================================ HEXCALC
      subroutine hexcalc(str,imode,ival)
! IMODE  0 - the later the character, the greater the value   <null>1 = 256
!        1 - the earlier the character, the greater the value  <null>1 = 1
!        2 - this is a string of hex numbers that I need to convert
!            first. I.E. bb should be 12 12

      character str*(*)

      idebug=isdebugactive("hexcalc")

      if(idebug==1)print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv hexcalc"

      il=len_trim(str)

      ival=0
      do i=0,il-1
       ie=i
       if(imode==1.or.imode==2)ie=(il-i)-1
       ic=ichar(str(i+1:i+1))
       if(idebug==1)print *,i,"-",ic
       if(imode==2)then
        if(ic<58)then
         ic=ic-48
        elseif(ic<91)then
         ic=ic-55
        else
         ic=ic-87
        endif
       endif
       im=256**ie
       if(imode==2)im=16**ie
       ival=ival+(im*ic)
!       print "(a,3i10)"," ic im ival: ",ic,im,ival
      enddo

      if(idebug==1)then
       print *,"returning: ",ival
       print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ hexcalc"
      endif
      
      return
      end subroutine

!========================= TIMEPASS
      subroutine timepass(imode,islot,rp,str)
! This is just like WALLTIME, but I needed double-precision reals
! so instead of rewrite everything that calls WALLTIME, I made
! this easier substitute.
! IMODE 1 - initialize   2 - how much time has passed since initialization?
! ISLOT - which timeslot to use?
      real*8,save:: t(100)
      real*8 ric,rir,rix,rinow,rnow,rmax,r8,rtmp
      real*8 rp
      character str*(*)
      data ifirst/1/
      if(ifirst==1)then; ifirst=0; t=-1.0; ifirst=0; endif
      if(imode==1)then
       CALL SYSTEM_CLOCK(IC,IR,IX)
       ric=ic; rir=ir
       t(islot)=ric/rir
       rp=t(islot)
      elseif(imode==2.or.imode==3)then
       if(imode==2)then
        if(t(islot)<0.0)call enditalli("Time slot not init'd: ",islot)
       endif
       CALL SYSTEM_CLOCK(INOW,IR,IX)
       rinow=inow; rir=ir; rix=ix
       rnow=rinow/rir
       rmax=rix/rir
       if(imode==2)then
        rtmp=t(islot)
       else
        rtmp=rp
       endif
       IF(rnow<rtmp)THEN
        r8=rmax-rtmp+rnow
       ELSE
        r8=RNOW-rtmp
        rp=r8*1.0
       ENDIF
       il=len_trim(str)
       if(il>0)print "(a,': ',f10.4)",trim(str),rp
      endif
      return
      end subroutine
      
      SUBROUTINE walltime(RVAL,IMODEIN,RPASS,tlab)
!***************************************************************
!  PURPOSE:         
!     THIS ROUTINE USES SYSTEM_CLOCK TO MARK THE PASSAGE OF
!     "WALL TIME", NOT JUST TIME SPENT ON THE CPU BECAUSE CPU
!     TIME DOESN'T INCLUDE FILE ACCESS TIME.
!     SYSTEM_CLOCK LOOKS LIKE THIS AND WILL RETURN...
!        CALL SYSTEM_CLOCK(IC,IR,IX)
!        IC - THE CURRENT NUMBER OF TICKS
!        IR - HOW MANY TICKS MAKE UP ONE SECOND
!        IX - THE ABSOLUTE MAXIMUM AMOUNT OF TICKS RETURNED. USING A MAX
!             PREVENTS THE NUMBER OF TICKS FROM BEING TOO BIG TO BE STORED IN
!             NORMAL INTEGER*4 VARIABLES. HOWEVER, THAT MEANS THAT EVERY 59
!             HOURS OR SO, THIS RESETS AND IT'S POSSIBLE TO HAVE A "START"
!             TIME LARGER THAN AN "END" TIME. BUT IT'S COOL. A SIMPLE
!             CALCULATION CAN BE DONE TO GET THE ELAPSED TICKS.
!
!  INPUT:           
!     RVAL  - WHEN IMODE=2 OR 3, THIS IS THE OLD NUMBER OF SECONDS
!     RPASS - WHEN IMODE=4, THIS IS THE NEW NUMBER OF SECONDS
!     IMODE - 1 - SET RVAL TO THE CURRENT NUMBER OF SECONDS
!             2 - USE RVAL AS THE OLD NUMBER OF SECONDS AND SET
!                 RPASS TO THE NUMBER OF SECONDS ELAPSED SINCE THEN
!             3 - USE RVAL AS THE OLD NUMBER OF SECONDS AND SET
!                 RPASS TO THE NUMBER OF SECONDS ELAPSED SINCE THEN,
!                 ALSO UPDATE RVAL TO CONTAIN THE CURRENT SECONDS
!             4 - RVAL AND RPASS HAVE ALREADY BEEN ASSIGNED START/STOP
!                 TIMES. SEND THEM THROUGH THE ELLAPSED TIME CALCULATION.
!
!  OUTPUT:          
!     RVAL  - WHEN IMODE=1 OR 3, THIS IS SET TO THE CURRENT NUMBER OF SECONDS
!     RPASS - WHEN IMODE=2, 3, OR 4, THIS IS THE SECONDS ELAPSED SINCE RVAL
!
!  METHOD:
!      IF IMODE=1 THEN I WANT TO GET A BASE NUMBER OF SECONDS
!         GET THE CURRENT NUMBER OF TICKS
!         DIVIDE TICKS BY TICK-RATE TO GET SECONDS
!         SET RVAL EQUAL TO THE NUMBER OF SECONDS
!      ELSEIF IMODE=2 OR 3, RETURN THE ELAPSED SECONDS SINCE RVAL
!         GET THE CURRENT NUMBER OF TICKS
!         DIVIDE TICKS BY TICK-RATE TO GET CURRENT-SECONDS
!         DIVIDE MAX-TICKS BY TICK-RATE TO GET MAX-SECONDS
!         IF CURRENT-SECONDS<RVAL THEN I PASSED MAX-SECONDS
!            RPASS IS MAX-SECONDS MINUS RVAL PLUS CUR-SECONDS
!         ELSE
!            RPASS IS CURRENT-SECONDS MINUS RVAL
!         ENDIF
!         IF IMODE=3, SET RVAL TO CUR-SECONDS
!      ENDIF
!                   
!   IMPLICITS       
!*************** START EXECUTABLE CODE ***************************  
!
      character tlab*(*)
      real*8 rval,rpass
      ineg=0
      if(imodein<0)ineg=1
      imode=abs(imodein)
      
      IF(IMODE.EQ.1)THEN
!
! RETURN THE BASE NUMBER OF SECONDS IN RVAL
!
       CALL SYSTEM_CLOCK(IC,IR,IX)
       RVAL=REAL(IC)/REAL(IR)
!
      ELSEIF(IMODE.EQ.2.OR.IMODE.EQ.3.OR.IMODE.EQ.4)THEN
!
! RETURN SECONDS ELAPSED SINCE RVAL
! FIND THE CURRENT TIME WITH INOW
!
       CALL SYSTEM_CLOCK(INOW,IR,IX)
!
! RNOW IS CURRENT SECONDS, RMAX IS MAX SECONDS
!
       RNOW=REAL(INOW)/REAL(IR)
       RMAX=REAL(IX)/REAL(IR)
       
       IF(IMODE==4)RNOW=RPASS
!
! THE CURRENT TIME !SHOULD! BE GREATER THAN THE OLD TIME. IF IT'S NOT,
! THEN THE MAX TIME WAS PASSED OVER. SUBTRACT OLD TIME FROM THE MAX
! TO GET THE COUNT IN BETWEEN, AND ADD THE CURRENT TIME TO GET TIME
! ELAPSED.
!
       IF(RNOW.LT.RVAL)THEN
        RPASS=RMAX-RVAL+RNOW
       ELSE
        RPASS=RNOW-RVAL
       ENDIF
!
       IF(IMODE.EQ.3)RVAL=RNOW
       if(ineg==1)print "(a,f10.3)",trim(tlab)//": ",rpass
!
      ENDIF
!
      RETURN
      END subroutine

!================================= INQUOTE
      function inquote(str,iloc)
! return if iloc is inside quotes
! str=test,string, iloc=5 would return 0
! str="test,string", iloc=5 would return 1
! str=test","string, iloc=6 would return 1

      character str*(*)
      il=len_trim(str)
      if(iloc>il)call enditall("INQUOTE: iloc > il")
      iqon=0
      do i=1,iloc
       if(str(i:i)=="'")then
        if(iqon/=39.and.iqon>0)goto 10
        if(iqon==39)then
         iqon=0
        else
         iqon=39
        endif
10      continue
       elseif(str(i:i)=='"')then
        if(iqon/=34.and.iqon>0)goto 20
        if(iqon==34)then
         iqon=0
        else
         iqon=34
        endif
20      continue
       endif
      enddo
      iret=0
      if(iqon>0)iret=1
      inquote=iret
      return
      end function
      
!=============================== IPARCNT
      function iparcnt(str,iloc)
! Return the depth of my paranthesis. For instance, if I look at the string
! "ARRAY(100,3)" at the comma location, 1 would be returned since I am inside
! of one parathesis. The string "A=1-((B-2)*4) at the location of "B" would
! return 2.
      character str*(*),c
      il=len_trim(str)
      if(iloc>il)call enditall("IPARCNT: STR longer than ILOC!")
      iret=0
      do i=1,iloc
       c=str(i:i)
       if(c=="(")iret=iret+1
       if(c==")")iret=iret-1
      enddo
      iparcnt=iret
      return
      end function
      
!=============================== IVAL2IDXSLOT
      function ival2idxslot(ival,idx,icnt)
      integer idx(icnt)
! this routine will find IVAL in IDX and return its slot
      iret=-1
      do i=1,icnt
       if(idx(i)==ival)then
        iret=i
        exit
       endif
      enddo
      ival2idxslot=iret
      return
      end function

!=============================== IVAL2IDXSLOTS
      function ival2idxslots(sval,sa,icnt)
      character*(*) sval,sa(1:icnt)
! this routine will find SVAL in SA and return its slot
      iret=-1
      do i=1,icnt
       if(trim(sa(i))==trim(sval))then
        iret=i
        exit
       endif
      enddo
      ival2idxslots=iret
      return
      end function

!============================= STALLTIME
      subroutine stalltime(rs)
      real*8 r1,r2
      call walltime(r1,1,r2,"")
      do
       call walltime(r1,2,r2,"")
       if(r2>=rs)exit
      enddo
      return
      end subroutine

!============================ INCI
      subroutine inci(i1,ii)
      real*8 rtimer
      call subtimer("inci",1,rtimer)
      i1=i1+ii
      call subtimer("inci",2,rtimer)
      return
      end subroutine
!============================ INCR
      subroutine incr(r1,rr)
      r1=r1+rr
      return
      end subroutine
!============================ INCR8
      subroutine incr8(r1,rr)
      real*8 rr,r1
      r1=r1+rr
      return
      end subroutine
!======================== PROGBAR
      subroutine progbar(icur,imaxval,imaxpt,icurpt)
!      use strutilmod
      character str*100,tm*5,s2*2,percstr*6,c1,tme*5,tmt*5
      real*8,save:: rstart
      character,save:: oproglab*100
      real*8 rdum,rpassed
      
      if(ignoprog==1)goto 99
! don't let the bar be too long      
      imaxptl=imaxpt
      if(imaxpt>50)imaxptl=50
      icurl=icur
! I should never go past imaxval, but JUST in case I do
! don't show it to preven the bar from being too long
      if(icur>imaxval)icurl=imaxval
!      if(icurl==-1)then
!       print "(a)","         1111111111222222222233333333334444444444555555555566666666667777777777"
!       print "(a)","1234567890123456789012345678901234567890123456789012345678901234567890123456789"
!      endif
      
      init=0
      if(icurl==-1)then
       icurl=0
       icurpt=0
       init=1
       call walltime(rstart,1,rdum,"")
! If I haven't set GPROGLAB yet, set it to spaces so nothing blows up
       if(ichar(gproglab(1:1))==0)gproglab=""
! pad GPROGLAB with - so it's easy to read with a fill bar       
       il=len_trim(gproglab)
       if(il>0)gproglab="-"//trim(gproglab)//"-"
      endif
      if(icurl==-2)then
       icurl=imaxval
       init=2
      endif
      
! If my label is longer than the bar, expand the bar       
      il=len_trim(gproglab)
      if(il>imaxptl)imaxptl=il+2
      
      rneed=real(imaxval)/real(imaxptl)
      if(abs(rneed)<.01)then
       ineedpt=0
      else
       ineedpt=icurl/rneed
      endif
      if(init==2)ineedpt=imaxptl
      if(ineedpt>icurpt.or.init==1.or.init==2.or.gproglab/=oproglab)then
       
       oproglab=gproglab
       call walltime(rstart,2,rpassed,"")
       icurpt=ineedpt
       rp=icurl/real(imaxval)
       rperc=rp*100.0
       
! RPASSED time has passed since I started and I'm RPERC finished.
! Use the following equation to figure out how much time is left.
! passed/tot = rp     -so-
! passed = rp*tot     -so-
! passed/rp = tot      will give me the total expected time.
!  for the first run, print a bunch of time.
       if(init==1)then; tm="Tons!"; tme="None!"; tmt="Tons!"; goto 10; endif
!  for the final run, print how much time elapsed
       if(init==2)then
        irs=rpassed+.5
        call sec2ms(irs*1.0,tme,3)
        tm="Done!"
        tmt=tme
        goto 10
       endif
       
       rtt=rpassed/rp
       rem=rtt-rpassed
20     continue        
! vvvv get the time remaining vvvv
       irs=rem+.5
       call sec2ms(irs*1.0,tm,3)
       irs=rpassed+.5
       call sec2ms(irs*1.0,tme,3)
       irs=rtt+.5
       call sec2ms(irs*1.0,tmt,3)
10     continue       
       str="["
       c1="."
       do i=1,imaxptl
        str(1+i:1+i)=c1
       enddo
       ipl=len_trim(gproglab)
       if(ipl>0)then
        ipl2=ipl/2
        ih=imaxptl/2
        str(ih-ipl2+2:ih-ipl2+ipl+1)=gproglab
       endif
! it would be nice to preserve the label, so only use a progress mark
! if I'm overlapping a "." or if this is the current spot of the mark.
       do i=1,ineedpt
        if(str(1+i:1+i)/=c1.and.i<ineedpt)goto 30
        str(1+i:1+i)="²"
30      continue        
       enddo
       write(percstr,"(f6.2)"),rperc
       str=trim(str)//"] "//percstr//"%  "//tme//" "//tm//" "//tmt
       il=len_trim(str)
       print "(a$)",trim(str)
! backspace to start of line
       do i=1,il
        print "(a$)",achar(8)
       enddo
       if(init==2)print "(a)",""
      endif
99    continue      
      return
      end subroutine

! ============================= PROGTOT
      subroutine progtot(str,iv,iov,imode)
!      use strutilmod
      character str*(*),ps*100,n*7
      if(iov==iv)goto 99
      iov=iv
      write(n,"(i7)"),iv
      ps=str//n
      if(imode==1)then
       print "(a)",trim(ps)
      else
       print "(a$)",trim(ps)
       ips=len_trim(ps)
       do i=1,ips
        print "(a$)",achar(8)
       enddo
      endif
99    continue
      return
      end subroutine

! ================================ SEC2MS
      subroutine sec2ms(rsec,tm,imode)
! IMODE 0 - ISEC is an integer (the way it always worked), tm will be 5 characters or less
!           in the format mm:ss. If time > 99:99, "Tons!" will be returned.
!       1 - ISEC is a real and I need to append 3 decimal places in the format mm:ss.MMM
!           If time > 99:99, "Tons!" will be returned.
!       2 - ISEC is an INT and I need to expand out to hours. There is no length limit to tm.
!       3 - Just like 0, but if a time is >99:99, I want to show the hours and minutes only, but
!           restrict the output to a 5 character string. For instance, 75 minutes and 32 seconds
!           will be shown like this "1h16m"
      character tm*(*),ms*3,ss*2,mm*2,hh*2
      ihh=0; imm=0; iss=0; ims=0
      ims=(mod(rsec,1.0)*1000.0)+0.5
      iss=int(rsec)
! If I don't care about MS, round the seconds up if necessary
      if(imode==0.or.imode==2.or.imode==3)then
       if(ims>=500)iss=iss+1
       ims=0
      endif
      do while(iss>=3600); iss=iss-3600; ihh=ihh+1; enddo
      do while(iss>=60); iss=iss-60; imm=imm+1; enddo
      
! For mode 0, I want to display up to 99 minutes. I can put an hour back onto minutes
! if the extra 60 minutes cause more than 99 minutes.
      if(imode==0.or.imode==1)then
       if(ihh>0.and.imm<=39)then
        ihh=ihh-1
        imm=imm+60
       endif
      endif
! For mode 3, if I have so much time that I can't display seconds, I need to round
! the seconds to the next minute, if necessaru
      if(imode==3)then
       if(ihh>0)then
        if(iss>=30)then
         imm=imm+1
         iss=0
         if(imm==60)then
          imm=0
          ihh=ihh+1
         endif
        endif
       endif
      endif
      
      if(ihh>99)then; tm="Tons!"; goto 10; endif

      write(ms,"(i3)"),ims
      write(ss,"(i2)"),iss
      write(mm,"(i2)"),imm
      write(hh,"(i2)"),ihh
      
      if(ms(1:1)==" ")ms(1:1)="0"
      if(ms(2:2)==" ")ms(2:2)="0"
      if(ss(1:1)==" ")ss(1:1)="0"
      
      if(imode==0)then
       tm=mm//":"//ss
       if(ihh>0)tm="Tons!"
      elseif(imode==1)then
       tm=mm//":"//ss//"."//ms
       if(ihh>0)tm="Tons!"
      elseif(imode==2)then
       if(mm(1:1)==" ")mm(1:1)="0"
       tm=hh//":"//mm//":"//ss
      elseif(imode==3)then
       tm=mm//":"//ss
       if(ihh>0)then
        if(mm(1:1)==" ")mm(1:1)="0"
        if(ihh>=10)then
         tm="Tons!"
        else
         tm=hh(2:2)//"h"//mm//"m"
        endif
       endif
      endif
10    continue      
       
      return
      end subroutine
      
! ================================ SEC2MS2
      subroutine sec2ms2(rsec,tm,imode)
! IMODE 0 - ISEC is an integer (the way it always worked), tm will be 5 characters or less
!           in the format mm:ss. If time > 99:99, "Tons!" will be returned.
!       1 - ISEC is a real and I need to append 3 decimal places in the format mm:ss.MMM
!           If time > 99:99, "Tons!" will be returned.
!       2 - ISEC is an INT and I need to expand out to hours. There is no length limit to tm.
!       3 - Just like 0, but if a time is >99:99, I want to show the hours and minutes only, but
!           restrict the output to a 5 character string. For instance, 75 minutes and 32 seconds
!           will be shown like this "1h16m"
      character tm*(*),s2*2,sms*3,hr*3,min*2
      if(imode==1)then
       rms=mod(rsec,1.0)
       rms=(rms*1000.0)+0.5
       irs=rsec
       write(sms,"(i3)"),ifix(rms)
       sms=adjustl(sms)
       if(len_trim(sms)==1)sms="0"//adjustl(sms)
       if(len_trim(sms)==2)sms="0"//adjustl(sms)
      else
       irs=rsec+0.5
      endif
! I have my remaining seconds, I need to turn that into a 5-
! character string in the format mm:ss
      irm=0
      do while(irs>59); irm=irm+1; irs=irs-60; enddo
! I'm gonna do some stuff that should be handled by STRUTILMOD, but I REALLY
! don't want this module dependent on STRUTILMOD
      irh=0
      if(imode==2.or.imode==3)then
       do while(irm>59); irh=irh+1; irm=irm-60; enddo
       write(hr,"(i3)"),irh
       if(imode==2)hr=trim(adjustl(hr))//":"
       if(imode==3)then
        hr=trim(adjustl(hr))//"h"
        if(irs>=30)irm=irm+1
        if(irm==60)then; irh=irh+1; irm=0; endif
       endif
      endif
      if(irm>99)then; tm="Tons!"; goto 10; endif
      write(s2,"(i2)"),irm
      min=s2
      if(irm<10)then
       if(imode==2.or.imode==3)then
        s2(1:1)="0"
       else
        s2(1:1)=" "
       endif
       min=s2
      endif
      tm=s2
      write(s2,"(i2)"),irs
      if(irs<10)s2(1:1)="0"
      tm=trim(tm)//":"//s2
      if(imode==1)tm=trim(tm)//"."//sms
      if(imode==2)tm=trim(hr)//trim(tm)
      if(imode==3)then
       tm=trim(hr)//min//"m"
       if(irh>99)tm="Tons!"
      endif
10    continue
      return
      end subroutine
      
! ================================= SLEEPMY
      subroutine sleepmy(rsec)
! The intrinsic SLEEP uses NO processer time, so I should use it for
! the integer part of the sleep time and my stuff for the rest.
      real*8 r1,r2
      rleft=mod(rsec,1.0)
      isleep=nint(rsec-rleft)
      call sleep(isleep)
      call walltime(r1,1,r2,"")
      do
       call walltime(r1,2,r2,"")
       if(r2>rleft)exit
      enddo
      return
      end subroutine
      
!=============================== IDEBUGFLY
      function idebugfly(fl)
      character fl*(*)
      iret=0
      open(file=fl,status="old",unit=68,iostat=istat)
      if(istat==0)then
       iret=1
       close(68,iostat=istat)
      endif
      idebugfly=iret
      return
      end function

!===================== SUBTIMER
      subroutine subtimer(sub,imode,rtime)
! IMODE - 1 - the start of the sub, start the timer.
!       - 2 - the end of the sub, increment the counter and add time
!       - 3 - just increment counter, don't track time.
!       - 4 & 5 - If I call a subroutine in another routine, but I don't
!                 want the time the second one takes to be logged to the
!                 first, I use 4 and 5. For example.
! SUB1 start
! start timing
! stuff that takes 2 seconds
! call SUB2 that takes 3 seconds
! end timing
! end subroutine
!
! Before, SUB1 would take 5 seconds and SUB2 takes 3 seconds.
! However,if I change the logic to this...
! ...
! call subtimer(SUB1,4,rdum)
! call SUB2 that takes 3 seconds
! call subtimer(SUB1,5,rdum)
! ...
! The time that SUB2 takes is subtracted from SUB1. This gives me a better
! review of how much time SUB1's core logic takes.

      data ifirst/1/
      data icnt/0/
      character sub*(*)
      real,save::rmaintime
      real*8 rtime,rdummy,rend
      logical lex
      
      if(igsubtime==0)goto 99
      
!      goto 99
      if(ifirst==1)then
!       do i=1,imaxsubcnt
        subtime%name=""
        subtime%time=0.0
        subtime%ravg=0.0
        subtime%cnt=0
        subtime%nottime=0.0
        subtime%icurmode=0
!       enddo
       ifirst=0
       open(file="callcount.txt",action="write",unit=11,iostat=istat)
       close(11,status="delete")
      endif
      
      if(imode==1)then
       call walltime(rtime,1,rdummy,"")
!  Since a program could end before kicking back to MAIN (ENDITALL, for example),
!  keep a local copy of the start time. When I call SUBTIMER from ENDITALL, I
!  will know what my start time was.
       if(sub=="main")rmaintime=rtime
       ix=ival2idxslots(sub,subtime(:icnt)%name,icnt)
       if(ix<=0)then
        icnt=icnt+1
        subtime(icnt)%name=sub
        subtime(icnt)%icurmode=1
       endif
      elseif(imode==2.or.imode==3)then
       if(sub=="main")rtime=rmaintime
       call walltime(rtime,2,rend,"")
       islot=ival2idxslots(sub,subtime(:icnt)%name,icnt)
       if(islot<=0)call enditall("Can't find needed SUB name!")
!10     continue
       subtime(islot)%cnt=subtime(islot)%cnt+1
       subtime(islot)%icurmode=imode
       if(imode==2)then
        subtime(islot)%time=subtime(islot)%time+rend
       elseif(imode==3)then
        subtime(islot)%time=-1
       endif
      elseif(imode==4)then
       call walltime(rtime,1,rdummy,"")
       iii=1
       islot=ival2idxslots(sub,subtime(:icnt)%name,icnt)
       subtime(islot)%icurmode=imode
      elseif(imode==5)then
       call walltime(rtime,2,rend,"")
       iii=1
       islot=ival2idxslots(sub,subtime(:icnt)%name,icnt)
       subtime(islot)%nottime=subtime(islot)%nottime+rend
      endif
      
! This is the end of the main routine. Sort the data to either list
! the longest times at the top, or the most calls at the top.
      if(imode==2.and.sub=="main")then
       if(icnt==0)goto 40
        do i=1,icnt
         subtime(i)%time=subtime(i)%time-subtime(i)%nottime
         subtime(i)%ravg=subtime(i)%time/subtime(i)%cnt
        enddo
        do i1=1,icnt-1
         do i2=i1+1,icnt
          if(igsubtime==1)then
!   sort by time
           ix=ivrelation(subtime(i1)%time,subtime(i2)%time,0.0001)
           if(ix==1)goto 20
           if(ix==-1)goto 30
           ix=ivrelation(subtime(i1)%cnt*1.0,subtime(i2)%cnt*1.0,0.1)
           if(ix==1)goto 20
           if(ix==-1)goto 30
           ix=ivrelation(subtime(i1)%ravg*1.0,subtime(i2)%ravg*1.0,0.0001)
           if(ix==1)goto 20
           if(ix==-1)goto 30
          elseif(igsubtime==2)then
!   sort by calls
           ix=ivrelation(subtime(i1)%cnt*1.0,subtime(i2)%cnt*1.0,0.1)
           if(ix==1)goto 20
           if(ix==-1)goto 30
           if(subtime(i1)%name>subtime(i2)%name)goto 20
           if(subtime(i1)%name<subtime(i2)%name)goto 30
           ix=ivrelation(subtime(i1)%time,subtime(i2)%time,0.0001)
           if(ix==1)goto 20
           if(ix==-1)goto 30
           ix=ivrelation(subtime(i1)%ravg*1.0,subtime(i2)%ravg*1.0,0.0001)
           if(ix==1)goto 20
           if(ix==-1)goto 30
          endif
          if(subtime(i1)%name>subtime(i2)%name)goto 20
          if(subtime(i1)%name<subtime(i2)%name)goto 30
          iii=1
          goto 30
20        continue
          subtimetmp=subtime(i1)
          subtime(i1)=subtime(i2)
          subtime(i2)=subtimetmp
30        continue
         enddo
        enddo
        inquire(file="callcount.txt",exist=lex)
        if(lex)call enditall("CALLCOUNT.TXT shouldn't exist!")
        open(file="callcount.txt",action="write",unit=11,iostat=istat)
        do i=1,icnt
         ravg=subtime(i)%ravg
         jcnt=subtime(i)%cnt
         rt=subtime(i)%time
         if(igsubtime==1.or.igsubtime==3)then
!    when sorting by time, print all info
          write(11,"(f10.3,' , ',i10,' , ',f12.5,' , ',a)"),rt,jcnt,ravg,trim(subtime(i)%name)
         elseif(igsubtime==2)then
!    when sorting by call counts, only print counts
          write(11,"(i10,' , ',a)"),jcnt,trim(subtime(i)%name)
         endif
        enddo
        close(11)
40      continue
      endif

99    continue
      return
      end subroutine

!======================== IQSRELATION
function iqsrelation(i1,i2)
! return the relationship between slots i1 and i2 across all
! quicksort variables. If I1>I2 return -1, if I2>I1 return 1,
! if they are tied, return 0.

! !!!BEWARE!!!! - This subroutine COULD be called many million times
! and is very costly. It should only be used for VERY complicated sorts.

! If I set INOTIE to 1, then if I have a tie in QSORT, it will stop.
! It's really meant for debugging purposes.
real*8 rsubtime
call subtimer("iqsrelation",1,rsubtime)
inotie=0
iret=0
if(i1<1.or.i1>imaxqsort)call enditalli("I1 messed up: ",i1)
if(i2<1.or.i2>imaxqsort)then
 call enditalli("I2 messed up: ",i2)
endif
ia1=iqsidx(i1)
ia2=iqsidx(i2)
if(iqshowmany==0)goto 99
! --- first value
if(iqsway1==1)then
 if(rqsv1(ia1)>rqsv1(ia2))then;iret=-1;goto 98;endif
 if(rqsv1(ia2)>rqsv1(ia1))then;iret=1;goto 98;endif
else
 if(rqsv1(ia1)>rqsv1(ia2))then;iret=1;goto 98;endif
 if(rqsv1(ia2)>rqsv1(ia1))then;iret=-1;goto 98;endif
endif
if(iqshowmany==1)goto 99
! --- second value
if(iqsway2==1)then
 if(rqsv2(ia1)>rqsv2(ia2))then;iret=-1;goto 99;endif
 if(rqsv2(ia2)>rqsv2(ia1))then;iret=1;goto 99;endif
else
 if(rqsv2(ia1)>rqsv2(ia2))then;iret=1;goto 99;endif
 if(rqsv2(ia2)>rqsv2(ia1))then;iret=-1;goto 99;endif
endif
if(iqshowmany==2)goto 99
! --- third value
if(iqsway3==1)then
 if(rqsv3(ia1)>rqsv3(ia2))then;iret=-1;goto 99;endif
 if(rqsv3(ia2)>rqsv3(ia1))then;iret=1;goto 99;endif
else
 if(rqsv3(ia1)>rqsv3(ia2))then;iret=1;goto 99;endif
 if(rqsv3(ia2)>rqsv3(ia1))then;iret=-1;goto 99;endif
endif
if(iqshowmany==3)goto 99
! --- third value
if(iqsway4==1)then
 if(rqsv4(ia1)>rqsv4(ia2))then;iret=-1;goto 99;endif
 if(rqsv4(ia2)>rqsv4(ia1))then;iret=1;goto 99;endif
else
 if(rqsv4(ia1)>rqsv4(ia2))then;iret=1;goto 99;endif
 if(rqsv4(ia2)>rqsv4(ia1))then;iret=-1;goto 99;endif
endif
if(iqshowmany==4)goto 99
99 continue
if(iqstxttb==1.and.iret==0)then
 if(tqsv1(ia1)>tqsv1(ia2))iret=-1
 if(tqsv1(ia2)>tqsv1(ia1))iret=1
endif
if(inotie==1)then
 if(iret==0)then
  call enditall("Tie in ISQRELATION!")
 endif
endif
98 continue
iqsrelation=iret
call subtimer("iqsrelation",2,rsubtime)
end function
!============================== QSORTI
subroutine qsorti(iway1,icnt,itb)
 real*8 rsubtime
 call subtimer("qsorti",1,rsubtime)
 iqstxttb=itb
 iqshowmany=1
 iqsway1=iway1
 call freshidx(iqsidx,icnt)
 do i=1,icnt
  rqsv1(i)=iqsv1(i)
 enddo
 call qsort(icnt)
 call subtimer("qsorti",2,rsubtime)
end subroutine
!============================== QSORTR
subroutine qsortr(iway1,icnt,itb)
 if(iway1<=0.or.iway1>=3)call enditalli("QSORTR11: Bad sort way!",iway1)
 iqstxttb=itb
 iqshowmany=1
 iqsway1=iway1
 call freshidx(iqsidx,icnt)
 call qsort(icnt)
end subroutine
!============================== QSORTRR
subroutine qsortrr(iway1,iway2,icnt,itb)
 if(iway1<=0.or.iway1>=3)call enditall("QSORTR21: Bad sort way!")
 if(iway2<=0.or.iway2>=3)call enditall("QSORTR22: Bad sort way!")
 iqstxttb=itb
 iqshowmany=2
 iqsway1=iway1
 iqsway2=iway2
 call freshidx(iqsidx,icnt)
 call qsort(icnt)
end subroutine
!============================== QSORTRRR
subroutine qsortrrr(iway1,iway2,iway3,icnt,itb)
 if(iway1<=0.or.iway1>=3)call enditall("QSORTR31: Bad sort way!")
 if(iway2<=0.or.iway2>=3)call enditall("QSORTR32: Bad sort way!")
 if(iway3<=0.or.iway3>=3)call enditall("QSORTR33: Bad sort way!")
 iqstxttb=itb
 iqshowmany=3
 iqsway1=iway1
 iqsway2=iway2
 iqsway3=iway3
 call freshidx(iqsidx,icnt)
 call qsort(icnt)
end subroutine
!============================== QSORTRRRR
subroutine qsortrrrr(iway1,iway2,iway3,iway4,icnt,itb)
 if(iway1<=0.or.iway1>=3)call enditall("QSORTR41: Bad sort way!")
 if(iway2<=0.or.iway2>=3)call enditall("QSORTR42: Bad sort way!")
 if(iway3<=0.or.iway3>=3)call enditall("QSORTR43: Bad sort way!")
 if(iway4<=0.or.iway4>=3)call enditall("QSORTR44: Bad sort way!")
 iqstxttb=itb
 iqshowmany=4
 iqsway1=iway1
 iqsway2=iway2
 iqsway3=iway3
 iqsway4=iway4
 call freshidx(iqsidx,icnt)
 call qsort(icnt)
end subroutine
!============================== QSORT
subroutine qsort(icnt)
! non-recursive quicksort
 integer,parameter:: qtmax=100
 integer ia(icnt),idx(icnt)
 real*8 rsubtime
      
 type qtype
  integer ist,ied
 end type
 type(qtype)qt(qtmax)
 call subtimer("qsort",1,rsubtime)

 iq=0
      
 ist=1; ied=icnt
 iii=1
!print *,"      qspart  vvvvvv  ",ist,ied
10 continue
ipiv=ist
j=ied+1
i=ist
do
 do
  j=j-1
 ! to prevent ties, don't compare same slot
  if(i==j)goto 20
  if(ipiv==j)exit
  ix=iqsrelation(ipiv,j)
  if(ix<=0)exit
 enddo
 do
  i=i+1
 ! to prevent ties, don't compare same slot
  if(i==j)goto 20
  if(ipiv==i)exit
  ix=iqsrelation(ipiv,i)
  if(ix>=0)exit
 enddo
20 continue 
 if(i<j)then
  call swapi(iqsidx(i),iqsidx(j))
 elseif(i==j)then
  call swapi(iqsidx(i),iqsidx(ipiv))
  ipiv=i
  inewpiv=i
  exit
 elseif(i>j)then
  call swapi(iqsidx(j),iqsidx(ipiv))
  ipiv=j
  inewpiv=j
  exit
 endif
enddo
!print *,"      qspart  ^^^^^^  ",ipivret
iii=1
if(qtmax-iq<=1)call enditall("Increase QTMAX!")
if(ied-inewpiv>=2)then
 iq=iq+1
 qt(iq)%ist=inewpiv+1
 qt(iq)%ied=ied
endif
if(inewpiv-ist>=2)then
 iq=iq+1
 qt(iq)%ist=ist
 qt(iq)%ied=inewpiv-1
 endif
if(iq==0)goto 30
      
ist=qt(iq)%ist
ied=qt(iq)%ied
qt(iq)%ist=0
qt(iq)%ied=0
iq=iq-1
goto 10
      
30    continue
iii=1
call subtimer("qsort",2,rsubtime)
return
end subroutine
!============================== INSSORTI
subroutine inssorti(iway1,icnt,itb)
 real*8 rsubtime
 call subtimer("inssorti",1,rsubtime)
 iqstxttb=itb
 iqshowmany=1
 iqsway1=iway1
 call freshidx(iqsidx,icnt)
 rqsv1(:icnt)=iqsv1(:icnt)
! do i=1,icnt
!  rqsv1(i)=iqsv1(i)
! enddo
 call inssort(icnt)
 call subtimer("inssorti",2,rsubtime)
end subroutine
!============================ INSSORT
subroutine inssort(icnt)
 real*8 rsubtime
 call subtimer("inssort",1,rsubtime)
   if(icnt>=imaxqsort)then
    print "(a)","INSSORT requires one extra spot to store values."
    call enditall("Increase IMAXQSORT!")
   endif
   iii=1
   imx=imaxqsort
   do i=2,icnt
   j=i-1
! since the index is very fluid, but the values I'm comparing
! need to stay the same, put the values for this index in
! the last slot of value arrays for a temp storage location.
!   call savetempval(i)
   iqsidx(imx)=iqsidx(i)
   do
!   calling IQSRELATION is NOT a good idea here because it will
!   be called a BUNCH of times. Inline comparisons are better.
!!!    ir=iqsrelation(j,imx)
!!!    if(ir>=1)exit
    ia=iqsidx(imx);ib=iqsidx(j)
!!    ir=ivrelation(rqsv1(ia),rqsv1(ib),0.0001)
!!    if(ir<=0)exit
    if(abs(rqsv1(ia)-rqsv1(ib))<=0.0001)exit
    if(iqsway1==1)then
     if(rqsv1(ia)>rqsv1(ib))exit
    else
     if(rqsv1(ia)<rqsv1(ib))exit
    endif
10  continue
    iqsidx(j+1)=iqsidx(j)
    j=j-1
    if(j<=0)exit
   enddo
   iqsidx(j+1)=iqsidx(imx)
   iii=1
  enddo
 call subtimer("inssort",2,rsubtime)
return
end subroutine
!============================= SAVETEMPVAL
subroutine savetempval(i)
rqsv1(imaxqsort)=rqsv1(i)
rqsv2(imaxqsort)=rqsv2(i)
rqsv3(imaxqsort)=rqsv3(i)
rqsv4(imaxqsort)=rqsv4(i)
iqsv1(imaxqsort)=iqsv1(i)
iqsv2(imaxqsort)=iqsv2(i)
tqsv1(imaxqsort)=tqsv1(i)
iqsidx(imaxqsort)=iqsidx(i)
return
end subroutine
!============================= GETTEMPVAL
subroutine gettempval(i)
rqsv1(i)=rqsv1(imaxqsort)
rqsv2(i)=rqsv2(imaxqsort)
rqsv3(i)=rqsv3(imaxqsort)
rqsv4(i)=rqsv4(imaxqsort)
iqsv1(i)=iqsv1(imaxqsort)
iqsv2(i)=iqsv2(imaxqsort)
tqsv1(i)=tqsv1(imaxqsort)
iqsidx(i)=iqsidx(imaxqsort)
return
end subroutine


subroutine oldqsorti(iway1,icnt)
 iqshowmany=1
 iqsway1=iway1
 call freshidx(iqsidx,icnt)
 do i=1,icnt
  rqsv1(i)=iqsv1(i)
 enddo
 call oldqsort(1,icnt)
end subroutine
!----
subroutine oldqsortii(iway1,iway2,icnt)
 iqshowmany=2
 iqsway1=iway1
 iqsway2=iway2
 call freshidx(iqsidx,icnt)
 do i=1,icnt
  rqsv1(i)=iqsv1(i)
  rqsv2(i)=iqsv2(i)
 enddo
 call oldqsort(1,icnt)
end subroutine
!----
subroutine oldqsortx(iway1,icnt,itb)
 iqstxttb=itb
 iqshowmany=0
 iqsway1=iway1
 call freshidx(iqsidx,icnt)
 call oldqsort(1,icnt)
end subroutine
!----
subroutine oldqsortr(iway1,icnt,itb)
 iqstxttb=itb
 iqshowmany=1
 iqsway1=iway1
 call freshidx(iqsidx,icnt)
 call oldqsort(1,icnt)
end subroutine
!----
subroutine oldqsortrr(iway1,iway2,icnt,itb)
 iqstxttb=itb
 iqshowmany=2
 iqsway1=iway1
 iqsway2=iway2
 call freshidx(iqsidx,icnt)
 call oldqsort(1,icnt)
end subroutine
!----
subroutine oldqsortrrr(iway1,iway2,iway3,icnt,itb)
 iqstxttb=itb
 iqshowmany=3
 iqsway1=iway1
 iqsway2=iway2
 iqsway3=iway3
 call freshidx(iqsidx,icnt)
 call oldqsort(1,icnt)
end subroutine
!----
subroutine oldqsortrrrr(iway1,iway2,iway3,iway4,icnt,itb)
 iqstxttb=itb
 iqshowmany=4
 iqsway1=iway1
 iqsway2=iway2
 iqsway3=iway3
 iqsway4=iway4
 call freshidx(iqsidx,icnt)
 call oldqsort(1,icnt)
end subroutine
!----
recursive subroutine oldqsort(ist,ied)
!print *,"qsort  vvvvvv  ",ist,ied,iass
 if(ist<1.or.ist>imaxqsort)call enditalli("IST messed up: ",ist)
 if(ied<1.or.ied>imaxqsort)call enditalli("IED messed up: ",ied)
 if(ist>=ied)goto 99
 call oldqspart(ist,ied,ipivret)
 if(ist<ipivret-1)call oldqsort(ist,ipivret-1)
 if(ipivret<ied)call oldqsort(ipivret,ied)
99 continue
!print *,"qsort  ^^^^^^  ",ist,ied
end subroutine
!----
subroutine oldqspart(ist,ied,ipivret)
 if(ist<1.or.ist>imaxqsort)call enditalli("IST messed up: ",ist)
 if(ied<1.or.ied>imaxqsort)call enditalli("IED messed up: ",ied)
!print *,"      qspart  vvvvvv  ",ist,ied
ipiv=ist
i=ist-1
j=ied+1
do
 j=j-1
 do
 ! to prevent ties, don't compare same slot
  if(ipiv==j)exit
  ix=iqsrelation(ipiv,j)
  if(ix<=0)exit
  j=j-1
 enddo
 i=i+1
 do
 ! to prevent ties, don't compare same slot
  if(ipiv==i)exit
  ix=iqsrelation(ipiv,i)
  if(ix>=0)exit
  i=i+1
 enddo
 if(i<j)then
  call swapi(iqsidx(i),iqsidx(j))
 elseif(i==j)then
  ipivret=i+1
  exit
 else
  ipivret=i
  exit
 endif
enddo
iii=1
!print *,"      qspart  ^^^^^^  ",ipivret
return
end subroutine
!----
!

!============================== MYFDATE
subroutine myfdate(d,iout)
! Fortran Intrinsic FDATE uses 24-hour time, but let's make that AM/PM.
! IOUT  0 - Tue Aug 29 2017 1:47p
!       1 - 170829

character d*(*),m,hh*2,yyyy*4,tt*10,d2*10
character*2,yy,mm,dd
il=len(d)
if(iout==1)then
 if(il<6)call enditall("MYFDATE: String D needs to be 6 characters to hold YYMMDD.")
 call date(d2)
 dd=d2(1:2)
 yy=d2(8:9)
 if(d2(4:6)=="JAN")mm="01"
 if(d2(4:6)=="FEB")mm="02"
 if(d2(4:6)=="MAR")mm="03"
 if(d2(4:6)=="APR")mm="04"
 if(d2(4:6)=="MAY")mm="05"
 if(d2(4:6)=="JUN")mm="06"
 if(d2(4:6)=="JUL")mm="07"
 if(d2(4:6)=="AUG")mm="08"
 if(d2(4:6)=="SEP")mm="09"
 if(d2(4:6)=="OCT")mm="10"
 if(d2(4:6)=="NOV")mm="11"
 if(d2(4:6)=="DEC")mm="12"
 d=yy//mm//dd
else
if(il<30)call enditall("MYFDATE: String D needs to be longer to hold date! 30 at least.")
call fdate(d)
ix=index(d,":")
if(ix<=0)goto 99
ix2=index(d(:ix)," ",.TRUE.)
if(ix2<=0)goto 99
read(d(ix2+1:ix2+2),"(i2)"),ih
m="a"
if(ih>=12)m="p"
if(ih>=13)ih=ih-12
ix=index(d,":",.TRUE.)
if(ix<=0)goto 99
d=d(:ix-1)//d(ix+2:)
d(ix:ix)=m
ix=index(d,":")
ix2=index(d(:ix)," ",.TRUE.)
write(hh,"(i2)"),ih
hh=adjustl(hh)
d=d(:ix2)//trim(hh)//d(ix:)
il=len_trim(d)
ix=index(d(:il)," ",.TRUE.)
yyyy=d(ix+1:)
d=d(:ix-1)
il=len_trim(d)
ix=index(d(:il)," ",.TRUE.)
tt=d(ix+1:)
d=d(:ix-1)
d=trim(d)//" "//trim(yyyy)//" "//trim(tt)
endif
iii=1
99 continue
return
end subroutine

!================================= IBITBOX
      function ibitbox(ival,ibit,imode)
! IMODE 1 - set bit to on
!       2 - set bit to off
!       3 - check if bit is set
      logical bt
      if(ibit>30.or.ibit<1)call enditalli("Bad Bit! ",ibit)
      iret=0
      if(imode==1)then
       iret=ibset(ival,ibit)
      elseif(imode==2)then
       iret=ibclr(ival,ibit)
      elseif(imode==3)then
       bt=btest(ival,ibit)
       if(bt)iret=1
       if(.not.bt)iret=0
      endif
      ibitbox=iret
      return
      end function

!======================== ICALCOR
function icalcor(i1,i2)
! Return the OR of i1 and i2
logical li1,li2
iret=0
do i=0,30
 li1=btest(i1,i)
 li2=btest(i2,i)
 if(li1.or.li2)then
  iret=ibitbox(iret,i,1)
 endif
enddo
icalcor=iret
return
end function

!======================== ICALCAND
function icalcand(i1,i2)
! Return the AND of i1 and i2
logical li1,li2
iret=0
do i=0,30
 li1=btest(i1,i)
 li2=btest(i2,i)
 if(li1.and.li2)then
  iret=ibitbox(iret,i,1)
 endif
enddo
icalcand=iret
return
end function

!=========================== SYSTEST
subroutine systest(iret)
! Return 1 if we're on MKS
!        2 if CYGWIN
character line*100
call system("pwd > 0systmp.txt")
open(file="0systmp.txt",unit=99)
read(99,"(a)"),line
close(unit=99,status="delete")
iret=1
if(line(:10)=="/cygdrive/")iret=2

!ifi=ifopen("curlinit.txt","R")
!call nextline(ifi,ieof)
!call fclose(-ifi)
return
end subroutine

!======================== PROGBAR2
      subroutine progbar2(icur,imaxval,imaxpt,icurpt)
      
      character*7 sperc
      character*78 sall,blankit
      character,save:: tplate*52,line*52
      real,save:: rdef,rperco
      real*8,save:: rstart,rup,rpass
      real*8 r2
      integer itext1,itext2
      data iblen/50/
      data iblen3/200/
      data idef/4/
      
      if(icur==-1)then
!  define the template
       tplate="[..................................................]"
       il=len_trim(gproglab)
       ifs=26-ifix(il/2.0)
       itext1=ifs
       itext2=ifs+il+1
       tplate(ifs:ifs)="-"
       tplate(ifs+1:ifs+il)=gproglab
       tplate(ifs+il+1:ifs+il+1)="-"
       call walltime(rstart,1,r2,"")
       rperc=0.0
       rperco=0.0
       icurpt=0
       il=len(sall)
       do i=1,il; blankit(i:i)=achar(8); enddo
       line=tplate
       goto 990
      endif
      
! At this point, I need to find if I need to update the progress bar.
      iii=1
      rperc=(icur*1.0)/imaxval
      ineedpt=nint(rperc*50.0)
! I want to do an update if the progress will change.
      if(ineedpt/=icurpt)goto 100
      
! If the % has changed, I want to update the progress, but only if
! a second has passed since I updated last.
      call walltime(rup,2,rpass,"")
      if(rpass<1.0)goto 999
      if(rperc-rperco<0.01)goto 999
      iii=1
      goto 990
      
      
100   continue
      
      line=tplate
      do i=1,ineedpt
       line(i+1:i+1)=achar(178)
      enddo
      
990   continue
      write(sperc,"(f7.2)"),rperc*100.0
      write(sall,"(a)")line//sperc//"%"
      print "(a$)",sall
      print "(a$)",blankit
      call walltime(rup,1,r2,"")
      icurpt=ineedpt
      rperco=rperc
999   continue
      return
      end subroutine
      
!========================== SETMIN
subroutine setmin(rv,r)
if(r<rv)rv=r
return
end subroutine

!========================== SETMAX
subroutine setmax(rv,r)
if(r>rv)rv=r
return
end subroutine

!========================== SETMINI
subroutine setmini(iv,i)
if(i<iv)iv=i
return
end subroutine

!========================== SETMAXI
subroutine setmaxi(iv,i)
if(i>iv)iv=i
return
end subroutine

!===================== RNORMDIST
function rnormdist(rul,rmean,rstd)
! This function works just like the Excel function NORMDIST, except that
! Excel gives you the area from -INF to RUL, but this function gives
! you the area from RUL to INF.
!http://onlinestatbook.com/2/calculators/normal_dist.html
if(abs(rstd)<.01)then
 call enditall("RSTD is 0, but it can't be!")
 rstd=.01
endif
rp=0.0
! How many standard deviations away is this?
rz=(rmean-rul)/rstd
if(rz<-7.0)then;rp=0.0;goto 999;endif
if(rz>7.0)then;rp=1.0;goto 999;endif
iflag=0
if(rz<0.0)iflag=1
rz=abs(rz)
rs=sqrt(2.0)/3.0*rz
rh=0.5
rb=0.0
do i=0,11
 ra=exp(-rh*rh/9.0)*sin(rh*rs)/rh
 rb=rb+ra
 rh=rh+1.0
enddo
rp=0.5-rb/gpi
if(iflag==0)rp=1.0-rp
999 continue
rnormdist=rp
return
end function

!============================ MINLOCMY
function minlocmy(ra,ist,ied,imode,rx)
! This function returns the subscript of RA that has the lowest
! value.
! IMODE - 0 - just return lowest subscript
!         1 - the value must be >= rx
!         2 - the value must be > rx
! I created this because I had some problems with MINLOC when
! I passed arrays like this... minloc(rm(5:10),1,rm>64.2).
! It would return incorrect information.
real ra(:)
real*8 r8

iret=0
rmm=15999999.0
do i=ist,ied
 if(imode==1.and.ra(i)<rx)cycle
 if(imode==2.and.ra(i)<=rx)cycle
 if(ra(i)<rmm)then
  iret=i
  rmm=ra(i)
 endif
enddo
minlocmy=iret
return
end function

!===================== SUBTIMEROLD
      subroutine subtimerold(sub,imode,rtime)
! IMODE - 1 - the start of the sub, start the timer.
! IMODE - 2 - the end of the sub, increment the counter and add time
! IMODE - 3 - just increment counter, don't track time.
      data ifirst/1/
      data icnt/0/
      character sub*(*)
      real,save::rmaintime
      real*8 rtime,rdummy,rend
      logical lex
      
!      goto 99
      if(ifirst==1)then
!       do i=1,imaxsubcnt
        subtime%name=""
        subtime%time=0.0
        subtime%ravg=0.0
        subtime%cnt=0
!       enddo
       ifirst=0
       open(file="callcount.txt",action="write",unit=11,iostat=istat)
       close(11,status="delete")
      endif
      
      if(igsubtime==0)goto 99
      
      if(imode==1)then
       call walltime(rtime,1,rdummy,"")
!  Since a program could end before kicking back to MAIN (ENDITALL, for example),
!  keep a local copy of the start time. When I call SUBTIMER from ENDITALL, I
!  will know what my start time was.
       if(sub=="main")rmaintime=rtime
      elseif(imode==2.or.imode==3)then
       if(sub=="main")rtime=rmaintime
       call walltime(rtime,2,rend,"")
       do i=1,icnt
        if(sub==subtime(i)%name)then
         islot=i
         goto 10
        endif
       enddo
       icnt=icnt+1
       if(icnt>imaxsubcnt)call enditalli("Increase IMAXSUBCNT: ",icnt)
       islot=icnt
       subtime(islot)%name=sub
10     continue
       subtime(islot)%cnt=subtime(islot)%cnt+1
       if(imode==2)then
        subtime(islot)%time=subtime(islot)%time+rend
       elseif(imode==3)then
        subtime(islot)%time=-1
       endif
      endif
      
! This is the end of the main routine. Sort the data to either list
! the longest times at the top, or the most calls at the top.
      if(imode==2.and.sub=="main")then
       if(icnt==0)goto 40
        do i=1,icnt
         subtime(i)%ravg=subtime(i)%time/subtime(i)%cnt
        enddo
        do i1=1,icnt-1
         do i2=i1+1,icnt
          if(igsubtime==1)then
!   sort by time
           ix=ivrelation(subtime(i1)%time,subtime(i2)%time,0.0001)
           if(ix==1)goto 20
           if(ix==-1)goto 30
           ix=ivrelation(subtime(i1)%cnt*1.0,subtime(i2)%cnt*1.0,0.1)
           if(ix==1)goto 20
           if(ix==-1)goto 30
           ix=ivrelation(subtime(i1)%ravg*1.0,subtime(i2)%ravg*1.0,0.0001)
           if(ix==1)goto 20
           if(ix==-1)goto 30
          elseif(igsubtime==2)then
!   sort by calls
           ix=ivrelation(subtime(i1)%cnt*1.0,subtime(i2)%cnt*1.0,0.1)
           if(ix==1)goto 20
           if(ix==-1)goto 30
           if(subtime(i1)%name>subtime(i2)%name)goto 20
           if(subtime(i1)%name<subtime(i2)%name)goto 30
           ix=ivrelation(subtime(i1)%time,subtime(i2)%time,0.0001)
           if(ix==1)goto 20
           if(ix==-1)goto 30
           ix=ivrelation(subtime(i1)%ravg*1.0,subtime(i2)%ravg*1.0,0.0001)
           if(ix==1)goto 20
           if(ix==-1)goto 30
          endif
          if(subtime(i1)%name>subtime(i2)%name)goto 20
          if(subtime(i1)%name<subtime(i2)%name)goto 30
          iii=1
          goto 30
20        continue
          subtimetmp=subtime(i1)
          subtime(i1)=subtime(i2)
          subtime(i2)=subtimetmp
30        continue
         enddo
        enddo
        inquire(file="callcount.txt",exist=lex)
        if(lex)call enditall("CALLCOUNT.TXT shouldn't exist!")
        open(file="callcount.txt",action="write",unit=11,iostat=istat)
        do i=1,icnt
         ravg=subtime(i)%ravg
         jcnt=subtime(i)%cnt
         rt=subtime(i)%time
         if(igsubtime==1)then
!    when sorting by time, print all info
          write(11,"(f10.3,' , ',i10,' , ',f12.5,' , ',a)"),rt,jcnt,ravg,trim(subtime(i)%name)
         elseif(igsubtime==2)then
!    when sorting by call counts, only print counts
          write(11,"(i10,' , ',a)"),jcnt,trim(subtime(i)%name)
         endif
        enddo
        close(11)
40      continue
      endif

99    continue
      return
      end subroutine
      
!================================ ODOMETER
      subroutine odometer(s,iway)
! S is a string that is to be treated like an odometer; it can be rolled
! forward or back and must loop when it has reached the end. I need to 
! determine if this is an numerical or alpha odometer (and then again if
! it's upper/lower case) and roll it correctly. Roll it the number of
! steps in IWAY. If IWAY is negative, roll it back.
! S is expected to be a "full" string with NO leading spaces!
      character s*(*),ct
      integer iv(100)
      il=len_trim(s)
      if(il>100.or.il<1)call enditall("ODOMETER: Bad S length.")
      if(s(1:1)==" ")call enditall("ODOMETER: Leading space not allowed.")
      if(iway==0)goto 99
!     Set IV array to individual characters in S
      do i=1,il
       iv(i)=ichar(s(i:i))
!      on the first character, determine what type of odometer this
!      is by setting min and max range
       if(i==1)then
        ix=iv(i)
        if(ix>=48)then
         imin=48; imax=57
        elseif(ix>=65)then
         imin=65; imax=90
        elseif(ix>=97)then
         imin=97; imax=122
        else
         call enditall("ODOMETER: Invalid first character in S: "//trim(s))
        endif
       endif
      enddo
      iw=iway
      inc=1
      if(iw<0)inc=-1
      iii=1
1     continue
! Start at the right-most digit and roll it one
      ix=il
2     continue
      iv(ix)=iv(ix)+inc
! If it's within, range, I'm done!
      if(iv(ix)>=imin.and.iv(ix)<=imax)goto 90
! If out of range, set it to min or max, depending on which way I'm
! rolling, and try to roll the next one
      if(iw>0)then
       iv(ix)=imin
      else
       iv(ix)=imax
      endif
      ix=ix-1
      if(ix==0)goto 90
      goto 2
90    continue
      if(iw<0)then
       iw=iw+1
      else
       iw=iw-1
      endif
      if(iw==0)goto 95
      goto 1
95    continue
      do i=1,il; s(i:i)=achar(iv(i)); enddo
      

99    continue
      return
      end subroutine
      
!======================================= POSIXFIX
      subroutine posixfix(s)
! Certain softwares don't like the unix path style of /CYGDRIVE/C/USER/...etc.../
! This routine changes it to a posix path with a drive letter
      character s*(*),t*5000
      
      t=s
      ix1=index(t,"/cygdrive/")
      ix2=index(t,"/CYGDRIVE/")
      ix=ix1
      if(ix2>ix)ix=ix2
! If I don't find cygdrive, nothing needs to change.
      if(ix<=0)goto 99
      s=t(11:11)//":"//t(12:)
      
99    continue
      
      return
      end subroutine
      
end module

