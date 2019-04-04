/**
 * *****************************************************************************
 * Copyright (c) 2014 Harmonia Holdings Group LLC and others. All rights
 * reserved. This program and the accompanying materials are made available
 * under the terms of the Harmonia Partner License v1.0 which accompanies this
 * distribution, and is available at http://www.harmonia.com/legal/hpl-v10.html
 *
 * Contributors: Harmonia Holdings Group LLC - initial API and implementation
 * *****************************************************************************
 */
package com.harmonia.scope;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * type GUI_To_Primary_Type is
 *
 * record -- commands Simulate_Secondary : Boolean; Forever : Boolean; Stop :
 * Boolean; Output_Interface_Data : Boolean; Time_To_Stop : Float; Debug_Level :
 * Short_Integer; Traffic_Simulation : Boolean; Start : Boolean;
 *
 * Intersection_Type : Utilities.Byte; Output_Protocol : Utilities.Byte;
 * New_Trace_On : Boolean; Spare_3 : Utilities.Byte;
 *
 * -- data New_Rc : Float; New_Yc : Float; New_Secondary_Display : Boolean;
 * New_Flash_1_Miscompare : Boolean; NTCIP : Boolean; Spare : Utilities.Byte;
 *
 * New_Splits : Utilities.Float_Array_16; New_Current_Colors :
 * Utilities.Two_Ring_Colors_Type; New_Mode : Short_Integer;
 *
 * New_Min_Green_Times : Utilities.Float_Array_16; New_Max_Green_Original :
 * Utilities.Float_Array_16; New_True_Max_Green_Times :
 * Utilities.Float_Array_16; New_Default_Extension_Times :
 * Utilities.Float_Array_16; New_Consecutive_Failures_Allowed : Integer;
 * New_Extension_Time_Increment : Float; New_Actuated_Mode :
 * Utilities.Byte_Array_16; New_Actuators : Utilities.Byte_Array_64;
 *
 * New_Gap_Times : Utilities.Float_Array_16; New_Times_Before_Reduction :
 * Utilities.Float_Array_16; New_Time_To_Reduce : Utilities.Float_Array_16;
 * New_Min_Gap_Times : Utilities.Float_Array_16; New_Ped_Walk_Times :
 * Utilities.Byte_Array_16; New_Ped_Clearance_Times : Utilities.Byte_Array_16;
 * New_Ped_Omit : Utilities.Byte_Array_16; New_Ped_Detector_Assignments :
 * Utilities.Byte_Array_16; New_Ped_Detector_Call : Utilities.Byte_Array_16;
 * Extra_Byte : Utilities.Byte;
 *
 * end record;
 */
/**
 *
 * @author bbadillo
 */
public class GUI2PrimaryData {

	// Possible Control types
	public final static short PRETIMED = 1000;
	public final static short ACTUATED = 1001;
	public final static short CICAS = 1002;
	public final static short LOW_LEVEL = 1003;
	public final static short ADAPTIVE = 1004;

	// Possible Colors
	public final static byte RED = 0;
	public final static byte YELLOW = 1;
	public final static byte GREEN = 2;

	// Possible Actuator Modes
	public final static byte PRESENCE_MODE = 0;
	public final static byte RECALL_MIN = 1;
	public final static byte RECALL_MAX = 2;
	public final static byte MAX_OUT = 3;
	public final static byte GAP_OUT = 4;
	public final static byte PEDESTRIAN_RECALL = 5;

	// Possible Actuator Values
	public final static byte NO_CALL_NO_CHANGE = 0;
	public final static byte HAS_CALL = 1;
	public final static byte NO_CALL_CHANGE = 2;
	public final static byte NEW_CALL = 3;
	
	/**
	 * Socket for communicating with SCOPE. 
	 */
	private Socket socket;
	
	// Block of data for output to SCOPE. 
	private Boolean Simulate_Secondary = false;
	private Boolean Forever = true;
	private Boolean Stop = false;
	private Boolean Output_Interface_Data = true;
	private Float Time_To_Stop = 0.0f;
	private Short Debug_Level = 2;
	private Boolean Traffic_Simulation = false;
	private Boolean Start = true;

	private Byte Intersection_Type = 0;
	private Byte Output_Protocol = 0;
	private Boolean New_Trace_On = false;
	private Byte Spare_3 = 0;

	private Float New_Rc = 2.0f;
	private Float New_Yc = 4.0f;
	private Boolean New_Secondary_Display = false;
	private Boolean New_Flash_1_Miscompare = false;
	private Boolean NTCIP = false;
	private Byte Spare = 0;

	private Float New_Splits[] = {0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f};
	private Byte New_Current_Colors[] = {RED, RED};
	private Short New_Mode = ACTUATED;

	private Float New_Min_Green_Times[] = {4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f, 4f};
	private Float New_Max_Green_Original[] = {60f, 60f, 60f, 60f, 60f, 60f, 60f, 60f, 60f, 60f, 60f, 60f, 60f, 60f, 60f, 60f};
	private Float New_True_Max_Green_Times[] = {70f, 70f, 70f, 70f, 70f, 70f, 70f, 70f, 70f, 70f, 70f, 70f, 70f, 70f, 70f, 70f};
	private Float New_Default_Extension_Times[] = {5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f};

	private Integer New_Consecutive_Failures_Allowed = 3;
	private Float New_Extension_Time_Increment = 1f;
	private Byte New_Actuated_Mode[] = {PRESENCE_MODE, RECALL_MIN, PRESENCE_MODE, PRESENCE_MODE, PRESENCE_MODE, RECALL_MIN, PRESENCE_MODE, PRESENCE_MODE,
		PRESENCE_MODE, PRESENCE_MODE, PRESENCE_MODE, PRESENCE_MODE, PRESENCE_MODE, PRESENCE_MODE, PRESENCE_MODE, PRESENCE_MODE};
	private Byte New_Actuators[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

	private Float New_Gap_Times[] = {5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f};
	private Float New_Times_Before_Reduction[] = {10f, 10f, 10f, 10f, 10f, 10f, 10f, 10f, 10f, 10f, 10f, 10f, 10f, 10f, 10f, 10f};
	private Float New_Time_To_Reduce[] = {5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f};
	private Float New_Min_Gap_Times[] = {2f, 2f, 2f, 2f, 2f, 2f, 2f, 2f, 2f, 2f, 2f, 2f, 2f, 2f, 2f, 2f};

	private Byte New_Ped_Walk_Times[] = {30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30};
	private Byte New_Ped_Clearance_Times[] = {10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10};
	private Byte New_Ped_Omit[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	private Byte New_Ped_Detector_Assignments[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	private Byte New_Ped_Detector_Call[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

	private Byte Extra_Byte = 0;

	/**
	 * Constructor. 
	 * 
	 * @throws IOException 
	 */
	public GUI2PrimaryData() throws IOException {
		socket = new Socket();
		socket.connect(new InetSocketAddress("127.0.01", 5878));
	}

	public void start() {
		sendData();
	}

	public void stop() {
		this.Stop = true;
		sendData();
	}
	
	public void actuate() {
		this.New_Actuators[0] = NEW_CALL;
		sendData();
	}

	private byte[] shortToByteArray(Short value) {
		ByteBuffer bytes = ByteBuffer.allocate(2);
		bytes.order(ByteOrder.LITTLE_ENDIAN);
		bytes.putShort(value);
		return bytes.array();
	}

	private byte[] floatToByteArray(Float value) {
		ByteBuffer bytes = ByteBuffer.allocate(4);
		bytes.order(ByteOrder.LITTLE_ENDIAN);
		bytes.putFloat(value);
		return bytes.array();
	}

	private byte[] integerToByteArray(Integer value) {
		ByteBuffer bytes = ByteBuffer.allocate(4);
		bytes.order(ByteOrder.LITTLE_ENDIAN);
		bytes.putInt(value);
		return bytes.array();
	}

	private void sendData() {
		try {

			OutputStream outputStream = socket.getOutputStream();
			DataOutputStream dos = new DataOutputStream(outputStream);

			dos.writeByte(this.Simulate_Secondary == true ? 1 : 0);//	Boolean Simulate_Secondary;
			dos.writeByte(this.Forever == true ? 1 : 0);//	Boolean Forever;
			dos.writeByte(this.Stop == true ? 1 : 0);//	Boolean Stop;
			dos.writeByte(this.Output_Interface_Data == true ? 1 : 0);//	Boolean Output_Interface_Data;

			dos.write(this.floatToByteArray(this.Time_To_Stop));//	Float Time_To_Stop;

			dos.write(this.shortToByteArray(this.Debug_Level));//	Short Debug_Level;
			dos.writeByte(this.Traffic_Simulation == true ? 1 : 0);//	Boolean Traffic_Simulation;
			dos.writeByte(this.Start == true ? 1 : 0);//	Boolean Start;

			dos.writeByte(this.Intersection_Type);//	Byte Intersection_Type;
			dos.writeByte(this.Output_Protocol);//	Byte Output_Protocol;
			dos.writeByte(this.New_Trace_On == true ? 1 : 0);//	Boolean New_Trace_On;
			dos.writeByte(this.Spare_3);//	Byte Spare_3;

			dos.write(this.floatToByteArray(this.New_Rc));//	New_Rc                   : Float ;
			dos.write(this.floatToByteArray(this.New_Yc));//	New_Yc                   : Float ;
			dos.writeByte(this.New_Secondary_Display == true ? 1 : 0);//	New_Secondary_Display    : Boolean ;
			dos.writeByte(this.New_Flash_1_Miscompare == true ? 1 : 0);//	New_Flash_1_Miscompare   : Boolean ;
			dos.writeByte(this.NTCIP == true ? 1 : 0);//	NTCIP                    : Boolean ;
			dos.writeByte(this.Spare);//	Spare                    : Utilities.Byte ;

			for (int i = 0; i < 16; i++) {
				dos.write(this.floatToByteArray(this.New_Splits[i]));//	New_Splits               : Utilities.Float_Array_16 ;
			}

			for (int i = 0; i < 2; i++) {
				dos.writeByte(this.New_Current_Colors[i]);//	New_Current_Colors       : Utilities.Two_Ring_Colors_Type ;
			}

			dos.write(this.shortToByteArray(this.New_Mode));//	New_Mode                 : Short_Integer ;
			
			for (int i = 0; i < 16; i++) {
				dos.write(this.floatToByteArray(this.New_Min_Green_Times[i]));//	New_Min_Green_Times              : Utilities.Float_Array_16 ;
			}
			for (int i = 0; i < 16; i++) {
				dos.write(this.floatToByteArray(this.New_Max_Green_Original[i]));//	New_Max_Green_Original           : Utilities.Float_Array_16 ;
			}
			for (int i = 0; i < 16; i++) {
				dos.write(this.floatToByteArray(this.New_True_Max_Green_Times[i]));//	New_True_Max_Green_Times         : Utilities.Float_Array_16 ;
			}
			for (int i = 0; i < 16; i++) {
				dos.write(this.floatToByteArray(this.New_Default_Extension_Times[i]));//	New_Default_Extension_Times      : Utilities.Float_Array_16 ;
			}
			dos.write(this.integerToByteArray(this.New_Consecutive_Failures_Allowed));//	New_Consecutive_Failures_Allowed : Integer ;
			dos.write(this.floatToByteArray(this.New_Extension_Time_Increment));//	New_Extension_Time_Increment     : Float ;
			for (int i = 0; i < 16; i++) {
				dos.writeByte(this.New_Actuated_Mode[i]);//	New_Actuated_Mode                : Utilities.Byte_Array_16 ;
			}
			for (int i = 0; i < 64; i++) {
				dos.writeByte(this.New_Actuators[i]);//	New_Actuators                    : Utilities.Byte_Array_64 ;
			}

			for (int i = 0; i < 16; i++) {
				dos.write(this.floatToByteArray(this.New_Gap_Times[i]));//	New_Gap_Times                    : Utilities.Float_Array_16 ;
			}
			for (int i = 0; i < 16; i++) {
				dos.write(this.floatToByteArray(this.New_Times_Before_Reduction[i]));//	New_Times_Before_Reduction       : Utilities.Float_Array_16 ;
			}
			for (int i = 0; i < 16; i++) {
				dos.write(this.floatToByteArray(this.New_Time_To_Reduce[i]));//	New_Time_To_Reduce               : Utilities.Float_Array_16 ;
			}
			for (int i = 0; i < 16; i++) {
				dos.write(this.floatToByteArray(this.New_Min_Gap_Times[i]));//	New_Min_Gap_Times                : Utilities.Float_Array_16 ;
			}

			for (int i = 0; i < 16; i++) {
				dos.writeByte(this.New_Ped_Walk_Times[i]);//	New_Ped_Walk_Times               : Utilities.Byte_Array_16 ;
			}
			for (int i = 0; i < 16; i++) {
				dos.writeByte(this.New_Ped_Clearance_Times[i]);//	New_Ped_Clearance_Times          : Utilities.Byte_Array_16 ;
			}
			for (int i = 0; i < 16; i++) {
				dos.writeByte(this.New_Ped_Omit[i]);//	New_Ped_Omit                     : Utilities.Byte_Array_16 ;
			}
			for (int i = 0; i < 16; i++) {
				dos.writeByte(this.New_Ped_Detector_Assignments[i]);//	New_Ped_Detector_Assignments     : Utilities.Byte_Array_16 ;
			}
			for (int i = 0; i < 16; i++) {
				dos.writeByte(this.New_Ped_Detector_Call[i]);//	New_Ped_Detector_Call            : Utilities.Byte_Array_16 ;
			}

			dos.writeByte(this.Extra_Byte);//	Extra_Byte                       : Utilities.Byte ;

			// Should be 776 bytes
			dos.flush();

		} catch (IOException ex) {
			Logger.getLogger(GUI2PrimaryData.class.getName()).log(Level.SEVERE, null, ex);
		}

	}

	/**
	 * Receive data from SCOPE and advance the 
	 */
	public void receiveData() {
		try {
			InputStream inputStream = socket.getInputStream();
			DataInputStream dis = new DataInputStream(inputStream);
			ByteBuffer bytes = ByteBuffer.allocate(4);
			bytes.order(ByteOrder.LITTLE_ENDIAN);
			
//		   --  Primary to GUI
//   type Display_Data_Type is record
//
			dis.readByte(); //      Display_Pri_Sec_Validation : Boolean                        := False;
			dis.readByte(); //      Display_Secondary_Status   : Boolean                        := False;
			dis.readByte();	//      Display_Status             : Utilities.Byte                 := 10;
			dis.readByte(); //      Display_Detector_Value     : Utilities.Byte                 := 16#FF#;
			dis.read(bytes.array(), 0, 4); //      Display_Split_Counter      : Float                          := 0.0;
			float splitCounter = bytes.getFloat();
			System.out.println("Split Counter: " + splitCounter);
			dis.readShort(); //      Display_Control_Mode       : Short_Integer                  := Atc_Api.Pretimed;
			dis.readShort(); //      Display_New_Control_Mode   : Short_Integer                  := Atc_Api.Pretimed;
			dis.readFloat(); //      Display_Rc                 : Float                          := 2.0;
			dis.readFloat(); //      Display_New_Rc             : Float                          := 2.0;
			dis.readFloat(); //      Display_Yc                 : Float                          := 2.0;
			dis.readFloat(); //      Display_New_Yc             : Float                          := 2.0;
//      Display_Current_Phases     : Utilities.Two_Ring_Phases_Type := (1,5);
			Byte ringPhase1 = dis.readByte();
			Byte ringPhase2 = dis.readByte();
//      Display_Current_Colors     : Utilities.Two_Ring_Colors_Type := (Utilities.Red,
//                                                                      Utilities.Red);
			Byte ringColor1 = dis.readByte();
			String ringColor1String = (ringColor1 == RED ? "RED" : (ringColor1 == GREEN ? "GREEN" : "YELLOW"));
			Byte ringColor2 = dis.readByte();
			String ringColor2String = (ringColor2 == RED ? "RED" : (ringColor2 == GREEN ? "GREEN" : "YELLOW"));
			
			System.out.println(String.format("Ring 1: Phase %s, Color %s", ringPhase1, ringColor1String));
			System.out.println(String.format("Ring 2: Phase %s, Color %s", ringPhase2, ringColor2String));
			
			dis.readByte(); //      Display_Miscompare_1       : Utilities.Byte                 := 0;
			dis.readByte(); //      Display_Miscompare_2       : Utilities.Byte                 := 0;
			dis.readByte(); //      Display_Miscompare_3       : Utilities.Byte                 := 0;
			dis.readByte(); //      Display_Miscompare_4       : Utilities.Byte                 := 0;
//      --  when these are for actuated logic, display splits represents the current time the
//      --  phase ends and the transition to the next phase occurs.  New splits represents the 
//      --  current computed maximum green times.
			for (int i = 0; i < 16; i++) {
				dis.readFloat(); //      Display_Splits             : Utilities.Float_Array_16   := (others => 0.0);
			}
			for (int i = 0; i < 16; i++) {
				dis.readFloat(); //      Display_New_Splits         : Utilities.Float_Array_16   := (others => 0.0);
			}
			dis.readByte(); //      Display_Current_Pedestrian : Boolean                    := False;
			dis.readByte(); //      Spare                      : Utilities.Byte             := 0;
//   end record;
		} catch (IOException ex) {
			Logger.getLogger(GUI2PrimaryData.class.getName()).log(Level.SEVERE, null, ex);
		}
	}
}
