/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
define({"77":{i:0.00278450385690753,u:"../Content/Simulations/Simulations/delete_rse.htm",a:"Delete an RSE Device To remove an RSE device from a composite, select the device in the Composite Settings dialog and choose Delete.  eTEXAS will display a confirmation message. Click Yes to delete the device, or No to keep it as is.",t:"Delete an RSE Device"},"78":{i:0.020164584300621,u:"../Content/Simulations/Composites/comp_reporting.htm",a:"A Composite can have one or more reporting applications. Select the Composite and choose Edit \u003e Reporting to open the  Hosted Applications  dialog. The dialog shows only reporting applications; choose an app from the Available column, and click \u003e to move it to the Hosted column. Multiple reporting ...",t:"Reporting"},"79":{i:0.00413557688799825,u:"../Content/Simulations/Simulations/simulations_about.htm",a:"Simulations  Pane A simulation is an imitation of the traffic and connected vehicle messaging events that can occur in a modeled intersection, side street, or other roadway. Simulations are grouped into Composites, which allow the traffic models for individual simulations to be combined into larger ...",t:"Simulations Pane"},"80":{i:0.00308035902092756,u:"../Content/Simulations/Simulations/create_sim_template.htm",a:"eTEXAS comes with several TEXAS simulation templates. Each template has lane geometry defined. In  Composite settings , you can add other components such as cell towers, detectors, fixed cellular devices, and RSE devices. Simulations are organized into Composites. Select the composite (or  create  ...",t:"Create Simulation from Template"},"81":{i:0.00278450385690753,u:"../Content/Simulations/Simulations/upload_sim.htm",a:"If you have previously exported a simulation or composite from eTEXAS, or have another simulation stored as a .zip, you can upload it here. Select (or create) the Composite that the uploaded simulation will go into.  In the Simulations pane, select Create \u003e from Upload. Complete these fields in the ...",t:"Upload a Simulation"},"82":{i:0.00278450385690753,u:"../Content/Simulations/Simulations/delete_sim.htm",a:"Delete a Simulation To delete a simulation, select the simulation name and choose Delete from the Simulations pane.  Choose Yes to confirm or No to cancel the deletion and return to the Simulations perspective.",t:"Delete a Simulation"},"83":{i:0.00278450385690753,u:"../Content/Simulations/Simulations/sim_edit_intro.htm",a:"Editing Simulations The Edit menu on the Simulations pane contains some basic functions, such as  copy ,  export , and  update .     Most settings cannot be changed if there is an existing  Execution  of the composite that contains the simulation. ",t:"Editing Simulations"},"84":{i:0.0033762141849476,u:"../Content/Simulations/Simulations/copy_sim.htm",a:"The ability to  copy a simulation allows you to easily duplicate a simulation in a different composite, or to simplify configuration when two simulations will have very similar settings. Unlike  exporting , a copy of a simulation retains all settings. You can then modify the settings as needed. If ...",t:"Copy a Simulation"},"85":{i:0.031021280557535,u:"../Content/Simulations/Simulations/export_sim.htm",a:"Export a simulation to a .zip file to store it, or share it with someone else. The simulation model (e.g. the TEXAS model / lane geometry) is exported; any settings that have been added are not included in the export. Select the simulation to be exported.   Select Edit \u003e Export in the Simulations ...",t:"Export a Simulation"},"86":{i:0.0166161181642535,u:"../Content/Simulations/Simulations/rename_sim.htm",a:"In the Update dialog, you can rename the simulation and/or change the X, Y location coordinates for  the simulation. Coordinates affect the placement of the intersections in a  visualization .  If all simulations are set to the same X and Y coordinates, the visualization places them on top of one ...",t:"Update a Simulation"},"87":{i:0.00278450385690753,u:"../Content/Simulations/Simulations/intro_detectors.htm",a:"Detectors are inductive-loop detectors embedded in pavement that can detect the presence of a vehicle. Specify the location of each detector using a lane number and distance (in centimeters) from the stop line. A detector can be placed on any inbound lane, whether there is a signal or not. Set the ...",t:"Detectors"},"88":{i:0.0059402922731212,u:"../Content/Simulations/Simulations/create_detector.htm",a:"Select the Simulation, then Edit \u003e Detectors. Click Create. Detectors are lane-specific, so you may need to create several detectors for a single simulation. The Create dialog will assist you in selecting lanes. Lane: specify the lane number; the drop-down list contains all possible lane numbers for ...",t:"Create a Detector"},"89":{i:0.00278450385690753,u:"../Content/Simulations/Simulations/edit_detector.htm",a:"To change the lane number and/or dimensions of a detector, select the detector and choose Edit. This opens the same dialog box that you used to  create  the detector.  Update: updates the detector with the values specified Reset: resets all fields to original values Cancel: closes the dialog without ...",t:"Edit a Detector"},"90":{i:0.00278450385690753,u:"../Content/Simulations/Simulations/delete_detector.htm",a:"Delete a Detector To remove a detector from a simulation, select the detector and choose Delete.  eTEXAS displays a confirmation message. Click Yes to delete the detector, or No to keep it as is.",t:"Delete a Detector"},"91":{i:0.00278450385690753,u:"../Content/Simulations/Simulations/source_files_sim.htm",a:"For simulations based on TEXAS models, when you select Source Files from the Simulations \u003e Edit menu, the files underlying the selected simulation are saved. They can then be opened in a TEXAS editor where you can make adjustments to the TEXAS model.  Consult documentation for TEXAS for further ...",t:"Edit Simulation Source Files"},"92":{i:0.0205961586192539,u:"../Content/Simulations/Composites/edit_parameter.htm",a:"In the  Hosted Applications  dialog, after you have assigned one or more applications, you may be able to change some parameters of the application(s).  Changing a value here does not modify the default value of a parameter, which may still be used in other instances of the application. The value is ...",t:"Edit a Parameter"},"93":{i:0.00278450385690753,u:"../Content/Dev_Guides/app_dev_guides.htm",a:"The primary purpose of the enhanced Traffic Experimental Analytical Simulation (eTEXAS) platform is to provide connected vehicle application developers with a tool to evaluate the performance of custom applications in a simulated traffic environment. The goal of the application development guides is ...",t:"Application Development Guides"},"94":{i:0.00488533580252853,u:"../Content/Dev_Guides/jar_dev_intro.htm",a:"JAR applications consist of one or more Java programs designed for the eTEXAS platform and packaged in a JAR file. eTEXAS supports JAR application deployment as a means for developers to write Java applications capable of processing and outputting information during executions without having to be ...",t:"Overview"},"95":{i:0.015863605395863,u:"../Content/Dev_Guides/jar_req_libraries.htm",a:"Required  Libraries Developing a custom JAR application for the eTEXAS platform requires the use of two libraries from the eTEXAS source code: generic-app and interrep-datamodel.  The generic-app library provides the following interfaces that define connected vehicle applications for each supported ...",t:"JAR Required Libraries"},"96":{i:0.0279892633540491,u:"../Content/Dev_Guides/jar_cv_interface.htm",a:"Implementing one of the connected vehicle application interfaces from the  generic-app library  requires adding the performUpdate method to your application class. The perform update method accepts the following parameters: T : device � the device hosting the application. Depending on which ...",t:"Implementing a Connected Vehicle App Interface"},"97":{i:0.0244835410093903,u:"../Content/Dev_Guides/jar_using_parameters.htm",a:"Using  Parameters in a JAR App Frequently, a developer wants to enable the behavior of a custom JAR application to be modified without changing the source code for the application. To support this functionality, eTEXAS uses application parameters. The first step in defining an application parameter ...",t:"Using Parameters in a JAR App"},"98":{i:0.0456582047502748,u:"../Content/Dev_Guides/jar_ex_tracker.htm",a:"The best way to learn about JAR application development for eTEXAS is to construct a simple JAR application to upload to the platform. As an example, we will construct an application that reports any vehicle traveling over a certain speed. The full source code for the application is available in  ...",t:"Sample Application: Speed Tracker"},"99":{i:0.00278450385690753,u:"../Content/Dev_Guides/jar_tracker_create.htm",a:"Speed Tracker: Step 1 Creating the Speed Tracker Project Open the Create a Java Project wizard by selecting File \u003e New \u003e Java Project from the main Eclipse menu bar. Note: you may need to choose File \u003e New \u003e Other� if Java Project is not available as an option. Enter Speed Tracker as the project ...",t:"Creating the Speed Tracker Project"},"100":{i:0.00278450385690753,u:"../Content/Dev_Guides/jar_tracker_libs.htm",a:"Speed Tracker: Step 2 Adding the Required Libraries Right click the name of the project in the Project Explorer and choose New \u003e Folder to create a folder for the required library files. Enter a suitable name for the folder and choose Finish. Copy the library JAR files into the newly created folder. ...",t:"Adding the Required Libraries"},"101":{i:0.00278450385690753,u:"../Content/Dev_Guides/jar_tracker_app_class.htm",a:"Speed Tracker: Step 3 Creating the Application Class Right click the src folder in the Project Explorer and choose New \u003e Package to create a package for the application class. Enter a suitable name for the package and choose Finish. Right click the new package in the Project Explorer and choose New ...",t:"Creating the Application Class"},"102":{i:0.00278450385690753,u:"../Content/Dev_Guides/jar_tracker_implement_interface.htm",a:"Speed Tracker: Step 4 Implementing an Application Interface For eTEXAS to recognize the application and its corresponding device type, we must implement one of the connected vehicle application interfaces described in  Required Libraries  and  Implementing a Connected Vehicle Application Interface . ...",t:"Implementing an Application Interface"},"103":{i:0.00278450385690753,u:"../Content/Dev_Guides/jar_tracker_parameter.htm",a:"Speed Tracker: Step 5 Adding a Speed Limit Parameter Allowing a user of our application to set a defined speed limit requires that we provide an  application parameter . To provide an application parameter, we must first define the field that will hold the speed limit value. Add the following line ...",t:"Adding a Speed Limit Parameter"},"104":{i:0.00278450385690753,u:"../Content/Dev_Guides/jar_tracker_logging.htm",a:"Speed Tracker: Step 6 Logging Application Output Reporting vehicles that exceed the defined speed limit requires that we check each vehicle in the simulator during the current time step. As a report application, the report device that is provided in the performUpdate method provides us with a way to ...",t:"Logging Application Output"},"105":{i:0.00278450385690753,u:"../Content/Dev_Guides/jar_tracker_naming.htm",a:"Speed Tracker: Step 7 Naming the Application By default, eTEXAS uses the class name of whichever classes implement a connected vehicle application interface as the name of applications uploaded from a JAR file. Although SpeedTracker is a perfectly suitable name for our application, let�s specify a ...",t:"Naming the Application"},"106":{i:0.00278450385690753,u:"../Content/Dev_Guides/jar_tracker_deploy.htm",a:"Speed Tracker: Step 8 Deploying the Application With a completed application, we can now deploy our application in eTEXAS. First, we must create a JAR file that can be uploaded:  Right click the project name in the Project Explorer.  Choose Export, select Java \u003e JAR file, and choose Next to continue ...",t:"Deploying the Application"},"107":{i:0.0416005068194461,u:"../Content/Dev_Guides/jar_tracker_sourcecode.htm",a:"SpeedTracker Source Code This page contains the source code for the  Speed Tracker sample  JAR application.  � package com.harmonia.speedtracker; � import java.util.Collection; import java.util.Iterator; � import org.etexascode.apps.IReportBaseApp; import org.etexascode.apps.ReportDevice; import ...",t:"Speed Tracker Source Code"},"108":{i:0.00515134516906778,u:"../Content/Dev_Guides/native_dev_intro.htm",a:"Overview TEXT�here for native apps development intro.",t:"Overview"},"109":{i:0.00278450385690753,u:"../Content/Intro Topics/faq.htm",a:"Frequently Asked Questions �",t:"Frequently Asked Questions"},"110":{i:0.00278450385690753,u:"../Content/Print_Title.htm",a:"� � �  Your Document Title Here �",t:"Updating the Harmonia Website"},"111":{i:0.00278450385690753,u:"../Content/test_adv_topic.htm",a:"test_adv_topic Delete this text and replace it with your own content.",t:"test_adv_topic"},"112":{i:0.00278450385690753,u:"../Content/Home.htm",a:"eTEXAS Help System\n Meet eTEXAS eTEXAS is a platform for Connected Vehicle App simulation. eTEXAS:  follows Connected Vehicle standards such as SAE J2735 messages allows Connected Vehicle Apps to send and receive DSRC/WAVE messages, and access data through DSRC units adds a wireless network ...",t:" eTEXAS Help System\n\t\t\t\t\t\t"},"113":{i:0.00278450385690753,u:"../Content/Examples/example_intro.htm",a:"Examples  Content to be added later.",t:"Examples "},"114":{i:0.00278450385690753,u:"../Content/pdf_toc.htm",a:"Table of Contents",t:"pdf_toc"},"115":{i:0.00278450385690753,u:"../Content/Applications/parameters.htm",a:"Application Parameters Delete this text and replace it with your own content.",t:"Application Parameters"},});
