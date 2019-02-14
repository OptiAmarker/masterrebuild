#include<stdlib.h>
#include<stdio.h>
#include<string.h>

char *avlloc_c(char[],FILE*);
void alphaplus1_c(char []);
void logit_c(char *,FILE *);

int globquiet,globqperf;
extern "C" void SETCGLOBS_C(int *q, int *qp)
{
   globquiet=*q;
   globqperf=*qp;
}

extern "C" int PRINTPOOP(int *a,char *str)
{
 int ret=88;
 FILE *f=fopen(str,"rb"),*fo=fopen("tmpo.tmp","wb");
 char *wholefile;
 long fz,loc;
 if(fseek(f,0,2)!=0)
 {
  printf("Bad FSEEK\n");
 }
 fz=ftell(f);
 rewind(f);
 wholefile=(char *)malloc((fz+1)*sizeof(char));
 wholefile[fz]='\0';
 fread(wholefile,sizeof(char),fz,f);
 fclose(f);

 loc=fz-1;
 while(1)
 {
  loc--;
  if(wholefile[loc]==10)
  {
	if(wholefile[loc+1]=='*')
	{
		break;
	}
	if(wholefile[loc+1]=='@')
	{
		break;
	}
  }
 }

 fputs(wholefile+8000,fo);
 fclose(fo);
 free(wholefile);
 printf("Hi there. %d  %s\n",*a,str);
 *a=66;
 return(ret);
}

extern "C" void FINFO_C(char *str, int *ierr, int *ipreshape, char *qseq, char *pname)
{
/*
Check for Q-file errors.
Errors include...
1 - No F-file information found.
2 - Invalid part name. A valid part name has at least one number or letter.
3 - Too many lines in F-file info.
4 - Not enough lines in F-file info.
5 - This is a SAN. This isn't an error, but this Q-file should
    NOT be used to rebuild F-files.
6 - The Q-file sequence in the file doesn't match the file name.
7 - This is a preshape, but the date is bad. Repair it!
99- Something BAD happened when opening the file.
*/

 *ierr=0;
 *ipreshape=0;
 FILE *f=fopen(str,"rb");
 if(f==NULL){*ierr=99; goto f20;}
 char *wholefile,*sc,line[1001];
 long fz,loc;
 int id,ilc=0,iltot,pos=0,iwlen,a,igood=0;
// printf("%s\n",str);
 if(fseek(f,0,2)!=0)
 {
  printf("Bad FSEEK\n");
 }
 fz=ftell(f);
 rewind(f);

 wholefile=(char *)malloc((fz+1)*sizeof(char));
 wholefile[fz]='\0';
 fread(wholefile,sizeof(char),fz,f);
 fclose(f);
 loc=fz-1;
 while(1)
 {
  loc--;
  if(loc<0){*ierr=1; goto f10;}
  if(strncmp("\n*",wholefile+loc,2)==0){break;}
  if(strncmp("\n@",wholefile+loc,2)==0){*ierr=5; goto f10;}
 }

// write F=file info to TMPO.TMP
/*
 FILE *fo=fopen("tmpo.tmp","wb");
 fputs(wholefile+loc+1,fo);
 fclose(fo);
*/ 

 iwlen=strlen(wholefile+loc+1);
// at this point, I know where the F-file stuff begins.
 while(1)
 {
    ilc++;
// find the end of the line
    strcpy(line,wholefile+loc+1+pos);
	 sc=strchr(line,10);
    id=sc-line;
	 pos+=id+1;
// copy the line to SC
    line[id]='\0';
// if there's a x13, remove it.
    if(line[id-1]==13)line[id-1]='\0';
// the string now contains only the info I need.
    id=strlen(line);
// do some error-checking depending on what line I'm on
    if(ilc==1)
 	 {
       if(id<73){*ierr=1;break;}
       strncpy(pname,line+1,40);
       strncpy(qseq,line+41,5);
       qseq[5]='\0';
       pname[40]='\0';
// Search the the Q-file sequence in the file name.
// There's a problem if they don't match.
       if(strstr(str,qseq)==NULL){*ierr=6;break;}
// Is this a preshape?
		 if(strncmp("*OPTI-PRETOOL-",line,14)==0)*ipreshape=1;
// Get the number of lines I SHOULD have in the tail.
       iltot=atoi(line+70);

       for(a=1;a<=40;a++)
       {
          if(line[a]>='0'&&line[a]<='9'){igood=1; break;}
          if(line[a]>='a'&&line[a]<='z'){igood=1; break;}
          if(line[a]>='A'&&line[a]<='Z'){igood=1; break;}
       }
       if(igood==0){*ierr=2;goto f10;}
	 }
    else if(ilc==4&&(*ipreshape)==1)
    {
// If this is a pretooled shape, make sure that the date is not ******
// That happens sometimes and it's bad.
       for(a=13;a<=16;a++)
       {
          if(line[a]<'0'||line[a]>'9'){*ierr=7;break;}
       }
    }
    if(ilc>1)
    {
       if(line[0]=='E'&&ilc<iltot){*ierr=3; break;}
       if(ilc>iltot){*ierr=3; break;}
       if(ilc==iltot&&line[0]!='E'&&ilc<iltot){*ierr=4; break;}
    }
    if(pos>=iwlen)
    {
       if(ilc<iltot)*ierr=3;
       break;
    }
 }
//
f10:;
 free(wholefile);
f20:;

}

#include<string>
#include<iostream>
#include<fstream>
using namespace std;
extern "C" void FINFO_CPP(char *c, int *ierr)
{
	*ierr=0;
	 ifstream file (c, ios::in|ios::binary|ios::ate);
	 std::string memblock,str;
	 size_t size=file.tellg(),ptr;
	 memblock.resize(size);
	 file.seekg(0,ios::beg);
	 file.read(&memblock[0],size);
	 file.close();
	 ptr=size;
	 while(1)
	 {
		 ptr--;
//		 str=memblock.substr(ptr-1,2);
		 if(memblock.substr(ptr-1,2)=="\n*")
		 {
			 ptr--;
			 break;
		 }
	 }

	 ofstream outfile;
	 outfile.open("tmpo.tmp",ios::out|ios::binary|ios::trunc);
	 outfile << memblock.substr(ptr+1);
	 outfile.close();
/*
	 streampos size;
 std::size_f ptr;
 size=file.tellg();
 memblock=new char[size];
 file.seekg(0,ios::beg);
 file.read (memblock, size);
 file.close();
 ptr=memblock.find("0");
 //std::string q="andrew";
 //std::size_t found;
 //found=q.find("n");
 //found=q.find("a");
 */
}

extern "C" void SPEEDTEST_C(char *c, int *ierr, char *back)
{
   FILE *f=fopen("output.txt","a");
   fprintf(f,"%s\n",c);
   fclose(f);
   strcpy(back,"hithere");
}

extern "C" void BUILDFFILES_C(int *iqcnt, char *gid, char *gext, char *lastf)
{
   FILE *f, *favl=fopen("avldisk.dat","r"), *ffil, *qfil, *logf=fopen("masterrebuild.log","a");
   int cnt,pos,fcnt,id,lpos,a;
   char q[6],gavldir[200],fstr[100],curf[6]="AAAAA",*wholefile,line[1001],*sc;
   long fz,loc;

   if((f=fopen("qorph.txt","rb"))==NULL)
   {
      printf("Bad file open.\n");
   }

   logit_c("\nCreating F-files...",logf);

// open an F file for writing
   strcpy(fstr,gid); strcat(fstr,"F"); strcat(fstr,curf);
   strcpy(gavldir,avlloc_c(fstr,favl));
   strcat(gavldir,fstr); strcat(gavldir,gext);
   strcpy(line,"  Writing ");
   strcat(line,fstr); strcat(line,gext); strcat(line,"...");
   logit_c(line,logf);
   ffil=fopen(gavldir,"w");
   fcnt=0;

   for(cnt=1;cnt<=*iqcnt;cnt++)
   {
// grab Q file from QORPH, find its location
      pos=(cnt-1)*6;
      if(fseek(f,pos,0)!=0)
      {
         printf("Bad FSEEK\n");
      }
      fgets(q,6,f);
      if(q[0]=='@')continue;
      strcpy(fstr,gid);
      strcat(fstr,"Q");
      strcat(fstr,q);
      strcpy(gavldir,avlloc_c(fstr,favl));
      strcat(gavldir,fstr);
      strcat(gavldir,gext);
// read q file to memory
      qfil=fopen(gavldir,"rb");
	  if(qfil==NULL)
	  {
		   lpos=111;
	  }
      if(fseek(qfil,0,2)!=0)
      {
         printf("Bad FSEEK\n");
      }
      fz=ftell(qfil);
      rewind(qfil);
      wholefile=(char *)malloc((fz+1)*sizeof(char));
      wholefile[fz]='\0';
      fread(wholefile,sizeof(char),fz,qfil);
      fclose(qfil);
// find tail informtaion
      lpos=0;
      loc=fz-1;
      while(1)
      {
       loc--;
       if(strncmp("\n*",wholefile+loc,2)==0){break;}
      }
// I've found the start of the tail in memory, but I need to handle
// it one line at a time.
      while(1)
      {
         strcpy(line,wholefile+loc+1+lpos);
         sc=strchr(line,10);
         if(sc==NULL)break;
         id=sc-line;
	      lpos+=id+1;
         line[id]='\0';
// if there's a x13, remove it.
         if(line[id-1]==13)line[id-1]='\0';
// trim trailing spaces so files are smaller.
         id=strlen(line);
         for(a=id-1;a>=0;a--)
         {
            if(line[a]!=' ')break;
            line[a]='\0';
         }
         fprintf(ffil,"%s\n",line);
      }

      fcnt++;
      if(fcnt==globqperf&&cnt<*iqcnt)
      {
         fputs("#\n",ffil);
         fclose(ffil);
         alphaplus1_c(curf);
// open an F file for writing
         strcpy(fstr,gid); strcat(fstr,"F"); strcat(fstr,curf);
         strcpy(gavldir,avlloc_c(fstr,favl));
         strcat(gavldir,fstr); strcat(gavldir,gext);
         ffil=fopen(gavldir,"w");
         strcpy(line,"  Writing ");
         strcat(line,fstr); strcat(line,gext); strcat(line,"...");
         logit_c(line,logf);
         fcnt=0;
      }
   }
   fputs("#\n",ffil);
   strcpy(lastf,curf);
   fclose(f);
   fclose(ffil);
   fclose(favl);
   fclose(logf);
}

char *avlloc_c(char f[], FILE *favl)
{
   int a,d1,d2;
   char l[200],f1[12],f2[12],dir[100];
   for(a=0;a<11;a++)
   {
    f1[a]=' ';
    f2[a]=' ';
   }
   f1[11]='\0';
   f2[11]='\0';
   rewind(favl);
   while(fgets(l,100,favl)!=NULL)
   {
      strncpy(dir,l+1,60);
      for(a=1;a<=60;a++)
      {
         if(dir[a]==' ')break;
      }
      dir[a]='\0';

      strncpy(f1,l+62,9);
      for(a=1;a<=9;a++){if(f1[a]==' ')break;}
      f1[a]='\0';

      strncpy(f2,l+71,9);
      for(a=1;a<=9;a++){if(f2[a]==' ')break;if(f2[a]==10)break;if(f2[a]==13)break;}
      f2[a]='\0';

      d1=strcmp(f,f1);
      d2=strcmp(f,f2);
      if(d1>=0&&d2<0)return dir;
   }
   return "./";
}

void alphaplus1_c(char a[])
{
   int cnt=4;
a10:;
   a[cnt]++;
   if(a[cnt]>90)
   {
      a[cnt]='A';
      cnt--;
      goto a10;
   }
}

void logit_c(char *str, FILE *f)
{
   if(globquiet==0)printf("%s\n",str);
   fprintf(f,"%s\n",str);
}