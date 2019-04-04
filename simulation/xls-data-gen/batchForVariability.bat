SET count=100
SET rep=1
SET tex=0
:loop_2
SET /A tex+=1
"C:\Program Files (x86)\Java\jdk1.7.0\bin\java.exe" -Djna.library.path="C:/texas/exe" -Drepnum="%rep%" -Ddsrcnum="%count%" -Dfilename="p%count%" -Dtexrep="%tex%" -jar "C:\Users\bbadillo\My Code\etexas\Simulation\VehicleWriter\target\VehicleWriter-1.0-SNAPSHOT-jar-with-dependencies.jar"
IF %tex% LSS 99 goto loop_2
