# Bugfix for autoconf 1.62-1.63 which broke $ac_link. Also add
# $ERLFLAGS to the run line.

# AC_LANG(Erlang)
# ---------------
m4_define([AC_LANG(Erlang)],
 [ac_ext=erl
 ac_compile='$ERLC $ERLCFLAGS -b beam conftest.$ac_ext >&AS_MESSAGE_LOG_FD'
 ac_link='$ERLC $ERLCFLAGS -b beam conftest.$ac_ext >&AS_MESSAGE_LOG_FD ; echo "[#]!/bin/sh" > conftest$ac_exeext ; echo "$ERL $ERLFLAGS -run conftest start -run init stop -noshell" >> conftest$ac_exeext ; chmod +x conftest$ac_exeext'
])

# Check if we can find Erlang header files.
AC_DEFUN([ERL_CHECK_HEADER],
	 [AC_MSG_CHECKING([for $1])
	  AC_LANG_PUSH(Erlang)
	  ac_objext=beam
	  AC_COMPILE_IFELSE(
		[AC_LANG_PROGRAM([[-include("$1").]],
				 [[ok]])],
		[AC_MSG_RESULT([ok])
		 $2],
		[AC_MSG_RESULT([not found])
		 $3])
	  AC_LANG_POP])

# Check if we can find an Erlang module.
AC_DEFUN([ERL_CHECK_MODULE],
	 [AC_MSG_CHECKING([for $1])
	  AC_LANG_PUSH(Erlang)
	  ac_objext=beam
 	  AC_RUN_IFELSE(
		[AC_LANG_PROGRAM([], [dnl
		     case code:which($1) of
		         non_existing ->
			     halt(1);
			 _ ->
			     halt()
	    	     end])],
		[AC_MSG_RESULT([ok])
		 $2],
		[AC_MSG_RESULT([not found])
		 $3])
 	  AC_LANG_POP])