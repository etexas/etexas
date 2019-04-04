SET count=-10
:loop_1
SET /A count+=10
SET rep=0
:loop_2
SET /A rep+=1
"C:\Program Files (x86)\Java\jdk1.7.0\bin\java.exe" -Djna.library.path="C:/texas/exe" -Drepnum="1" -Ddsrcnum="%count%" -Dfilename="p%count%r%rep%" -Dsplitphase="yes" -Drandomseed="%rep%" -jar "C:\Users\bbadillo\My Code\etexas\Simulation\QueueLengthData\target\QueueLengthData-1.0-SNAPSHOT-jar-with-dependencies.jar"
IF %rep% LSS 100 goto loop_2
IF %count% LSS 100 goto loop_1
