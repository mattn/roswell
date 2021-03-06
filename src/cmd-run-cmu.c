/* -*- tab-width : 2 -*- */
#include "opt.h"

char** cmd_run_cmu(int argc,char** argv,struct sub_command* cmd) {
  char** arg=NULL;
  char* home=configdir();
  char* arch=uname_m();
  char* os=uname();
  char* impl=(char*)cmd->name;
  char* version=(char*)cmd->short_name;
  int offset=10; /*[binpath for lisp] -quiet -core param -eval init.lisp
                  -noinit -nositeinit [terminating NULL] that total 9 are default. */
  int i;
  if(strcmp(impl,"cmu"))
    impl="cmucl";
  char* impl_path= cat(home,"impls",SLASH,arch,SLASH,os,SLASH,impl,SLASH,version,NULL);
  char* help=get_opt("help",0);
  char* script=get_opt("script",0);
  char* image=get_opt("image",0);
  char* program=get_opt("program",0);
  char* dynamic_space_size=get_opt("dynamic-space-size",0);
  char* control_stack_size=get_opt("control-stack-size",0);
  char* cmu_version=get_opt("version",0);
  int paramc=0;
  char *bin;
  int issystem=(strcmp("system",version)==0);
  if(issystem){
    bin=which("lisp");
    if(strcmp(bin,"")==0)
      s(bin),bin=which("cmucl");
    bin=truename(bin);
  }else {
    bin=cat(impl_path,SLASH,"bin",SLASH,"lisp",EXE_EXTENTION,NULL);
  }
  if(dynamic_space_size)
    offset+=2;
  if(control_stack_size)
    offset+=2;
  if(cmu_version)
    offset+=2;
  if(script)
    offset+=1;
  if(quicklisp)
    offset+=2;
  if(program||script)
    offset+=2;

  arg=alloc(sizeof(char*)*(offset+argc));
  arg[paramc++]=q("wrapper-dummy");
  arg[paramc++]=bin;
  /* runtime options from here */

  arg[paramc++]=q("-quiet");
  if(image) {
    char *path=cat(impl_path,SLASH,"dump",SLASH,image,".core",NULL);
    arg[paramc++]=q("-core");
    if(file_exist_p(path)) {
      arg[paramc++]=path;
    }else {
      cond_printf(1,"core not found:%s\n",path);
      arg[paramc++]=cat(impl_path,SLASH,"lib",SLASH,"sbcl",SLASH,"sbcl.core",NULL);
      s(path);
    }
  }
  if(help) {
    arg[paramc++]=q("-help");
  }

  arg[paramc++]=q("-noinit");
  arg[paramc++]=q("-nositeinit");

  if(dynamic_space_size) {
    arg[paramc++]=q("-dynamic-space-size");
    arg[paramc++]=q(dynamic_space_size);
  }
  if(control_stack_size) {
    arg[paramc++]=q("-control-stack-size");
    arg[paramc++]=q(control_stack_size);
  }
  if(cmu_version) {
    arg[paramc++]=q("-eval");
    arg[paramc++]=q("(progn (format t \"~A ~A~%\" (lisp-implementation-type) (lisp-implementation-version))(extensions:quit))");
  }
  arg[paramc++]=q("-eval");
  arg[paramc++]=s_cat(q("(progn(setq *load-verbose*()*compile-verbose*())#-ros.init(cl:load \""),s_escape_string(lispdir()),q("init.lisp"),q("\"))"),NULL);

  if(quicklisp) {
    arg[paramc++]=q("-eval");
    arg[paramc++]=q("(ros:quicklisp)");
  }
  if(program || script) {
    char *tmp;
    arg[paramc++]=q("-eval");
    tmp=cat("(ros:run '(",program?program:"",script?"(:script ":"",script?script:"",script?")":"",script?"(:quit ())":"","))",NULL);
    arg[paramc++]=tmp;
  }

  for(i=1;i<argc;++i) {
    arg[paramc++]=argv[i];
  }

  s(impl_path);

  arg[paramc]=NULL;
  cond_printf(1,"\nhelp=%s script=%s\n",help?"t":"nil"
              ,script?script:"nil");
  return arg;
}
