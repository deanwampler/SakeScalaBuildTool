::#!
@echo off
rem Warning: untested!
call scala 
goto :eof
::!#
:load sake.scala
build("%*")
