SET count=-10
:loop_1
SET /A count+=10
"C:\Program Files (x86)\Java\jdk1.7.0\bin\java.exe" -Djna.library.path="C:/texas/exe" -Drepnum="1" -Ddsrcnum="%count%" -Dfilename="p%count%" -jar "C:\Users\bbadillo\My Code\etexas\Simulation\VehicleWriter\target\VehicleWriter-1.0-SNAPSHOT-jar-with-dependencies.jar"
IF %count% LSS 100 goto loop_1
