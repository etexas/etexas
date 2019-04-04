/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.webapp.ejb;

/**
 * Provides query name constants.
 * 
 * @author emyers
 * @author ttevendale
 */
final class Query {

    /**
     * The query to retrieve the unique applications from application logs with the specified
     * execution.
     * 
     * @see QueryParameter#EXECUTION_ID
     */
    static final String APPLICATION_LOG_APPLICATIONS_FROM_EXECUTION_ID = "getApplicationLogApplications(executionId)";

    /**
     * The query to retrieve the unique applications from application logs with the specified
     * execution. The results are restricted to log entries for the devices in the provided list.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     */
    static final String APPLICATION_LOG_APPLICATIONS_FROM_EXECUTION_ID_DEVICE_LIST = "getApplicationLogApplications(executionId, deviceList)";

    /**
     * The query to retrieve the number of application logs with the specified execution. The
     * results are restricted to log entries for the provided minimum and maximum range of
     * simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_MIN_TIME_MAX_TIME = "getApplicationLogCount(executionId, minTime, maxTime)";

    /**
     * The query to retrieve the number of application logs with the specified execution. The
     * results are restricted to log entries for the devices in the specified list as well as the
     * provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_DEVICE_LIST_MIN_TIME_MAX_TIME = "getApplicationLogCount(executionId, deviceList, minTime, maxTime)";

    /**
     * The query to retrieve the number of application logs with the specified execution. The
     * results are restricted to log entries for the applications in the specified list as well as
     * the provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#APPLICATION_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_APPLICATION_LIST_MIN_TIME_MAX_TIME = "getApplicationLogCount(executionId, applicationList, minTime, maxTime)";

    /**
     * The query to retrieve the number of application logs with the specified execution. The
     * results are restricted to log entries for the keys in the specified list as well as the
     * provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#KEY_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_KEY_LIST_MIN_TIME_MAX_TIME = "getApplicationLogCount(executionId, keyList, minTime, maxTime)";

    /**
     * The query to retrieve the number of application logs with the specified execution. The
     * results are restricted to log entries for the devices and applications in the specified lists
     * as well as the provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     * @see QueryParameter#APPLICATION_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_DEVICE_LIST_APPLICATION_LIST_MIN_TIME_MAX_TIME = "getApplicationLogCount(executionId, deviceList, applicationList, minTime, maxTime)";

    /**
     * The query to retrieve the number of application logs with the specified execution. The
     * results are restricted to log entries for the devices and keys in the specified lists as well
     * as the provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     * @see QueryParameter#KEY_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_DEVICE_LIST_KEY_LIST_MIN_TIME_MAX_TIME = "getApplicationLogCount(executionId, deviceList, keyList, minTime, maxTime)";

    /**
     * The query to retrieve the number of application logs with the specified execution. The
     * results are restricted to log entries for the applications and keys in the specified lists as
     * well as the provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#APPLICATION_LIST
     * @see QueryParameter#KEY_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_APPLICATION_LIST_KEY_LIST_MIN_TIME_MAX_TIME = "getApplicationLogCount(executionId, applicationList, keyList, minTime, maxTime)";

    /**
     * The query to retrieve the number of application logs with the specified execution. The
     * results are restricted to log entries for the devices, applications, and keys in the
     * specified lists as well as the provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     * @see QueryParameter#APPLICATION_LIST
     * @see QueryParameter#KEY_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOG_COUNT_FROM_EXECUTION_ID_DEVICE_LIST_APPLICATION_LIST_KEY_LIST_MIN_TIME_MAX_TIME = "getApplicationLogCount(executionId, deviceList, applicationList, keyList, minTime, maxTime)";

    /**
     * The query to retrieve the unique devices from application logs with the specified execution.
     * 
     * @see QueryParameter#EXECUTION_ID
     */
    static final String APPLICATION_LOG_DEVICES_FROM_EXECUTION_ID = "getApplicationLogDevices(executionId)";

    /**
     * The query to retrieve the unique keys from application logs with the specified execution.
     * 
     * @see QueryParameter#EXECUTION_ID
     */
    static final String APPLICATION_LOG_KEYS_FROM_EXECUTION_ID = "getApplicationLogKeys(executionId)";

    /**
     * The query to retrieve the unique keys from application logs with the specified execution. The
     * results are restricted to log entries for the devices in the provided list.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     */
    static final String APPLICATION_LOG_KEYS_FROM_EXECUTION_ID_DEVICE_LIST = "getApplicationLogKeys(executionId, deviceList)";

    /**
     * The query to retrieve the unique keys from application logs with the specified execution. The
     * results are restricted to log entries for the applications in the provided list.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#APPLICATION_LIST
     */
    static final String APPLICATION_LOG_KEYS_FROM_EXECUTION_ID_APPLICATION_LIST = "getApplicationLogKeys(executionId, applicationList)";

    /**
     * The query to retrieve the unique keys from application logs with the specified execution. The
     * results are restricted to log entries for the devices and applications in the provided lists.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     * @see QueryParameter#APPLICATION_LIST
     */
    static final String APPLICATION_LOG_KEYS_FROM_EXECUTION_ID_DEVICE_LIST_APPLICATION_LIST = "getApplicationLogKeys(executionId, deviceList, applicationList)";

    /**
     * The query to retrieve the application logs with the specified execution. The results are
     * restricted to log entries for the provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOGS_FROM_EXECUTION_ID_MIN_TIME_MAX_TIME = "getApplicationLogs(executionId, minTime, maxTime)";

    /**
     * The query to retrieve the application logs with the specified execution. The results are
     * restricted to log entries for the devices in the specified list as well as the provided
     * minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOGS_FROM_EXECUTION_ID_DEVICE_LIST_MIN_TIME_MAX_TIME = "getApplicationLogs(executionId, deviceList, minTime, maxTime)";

    /**
     * The query to retrieve the application logs with the specified execution. The results are
     * restricted to log entries for the applications in the specified list as well as the provided
     * minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#APPLICATION_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOGS_FROM_EXECUTION_ID_APPLICATION_LIST_MIN_TIME_MAX_TIME = "getApplicationLogs(executionId, applicationList, minTime, maxTime)";

    /**
     * The query to retrieve the application logs with the specified execution. The results are
     * restricted to log entries for the keys in the specified list as well as the provided minimum
     * and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#KEY_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOGS_FROM_EXECUTION_ID_KEY_LIST_MIN_TIME_MAX_TIME = "getApplicationLogs(executionId, keyList, minTime, maxTime)";

    /**
     * The query to retrieve the application logs with the specified execution. The results are
     * restricted to log entries for the devices and applications in the specified lists as well as
     * the provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     * @see QueryParameter#APPLICATION_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOGS_FROM_EXECUTION_ID_DEVICE_LIST_APPLICATION_LIST_MIN_TIME_MAX_TIME = "getApplicationLogs(executionId, deviceList, applicationList, minTime, maxTime)";

    /**
     * The query to retrieve the application logs with the specified execution. The results are
     * restricted to log entries for the devices and keys in the specified lists as well as the
     * provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     * @see QueryParameter#KEY_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOGS_FROM_EXECUTION_ID_DEVICE_LIST_KEY_LIST_MIN_TIME_MAX_TIME = "getApplicationLogs(executionId, deviceList, keyList, minTime, maxTime)";

    /**
     * The query to retrieve the application logs with the specified execution. The results are
     * restricted to log entries for the applications and keys in the specified lists as well as the
     * provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#APPLICATION_LIST
     * @see QueryParameter#KEY_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOGS_FROM_EXECUTION_ID_APPLICATION_LIST_KEY_LIST_MIN_TIME_MAX_TIME = "getApplicationLogs(executionId, applicationList, keyList, minTime, maxTime)";

    /**
     * The query to retrieve the application logs with the specified execution. The results are
     * restricted to log entries for the devices, applications, and keys in the specified lists as
     * well as the provided minimum and maximum range of simulation times.
     * 
     * @see QueryParameter#EXECUTION_ID
     * @see QueryParameter#DEVICE_LIST
     * @see QueryParameter#APPLICATION_LIST
     * @see QueryParameter#KEY_LIST
     * @see QueryParameter#MIN_TIME
     * @see QueryParameter#MAX_TIME
     */
    static final String APPLICATION_LOGS_FROM_EXECUTION_ID_DEVICE_LIST_APPLICATION_LIST_KEY_LIST_MIN_TIME_MAX_TIME = "getApplicationLogs(executionId, deviceList, applicationList, keyList, minTime, maxTime)";

    /**
     * The query to retrieve the application parameters with the specified ID, application,
     * composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#APPLICATION_ID
     * @see QueryParameter#APPLICATION_PARAMETER_ID
     */
    static final String APPLICATION_PARAMETERS_FROM_USER_ID_COMPOSITE_ID_APPLICATION_ID_APPLICATION_PARAMETER_ID = "getApplicationParameters(userId, compositeId, applicationId, applicationParameterId)";

    /**
     * The query to retrieve the application parameters with the specified ID, application, device,
     * composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_ID
     * @see QueryParameter#APPLICATION_ID
     * @see QueryParameter#APPLICATION_PARAMETER_ID
     */
    static final String APPLICATION_PARAMETERS_FROM_USER_ID_COMPOSITE_ID_DEVICE_ID_APPLICATION_ID_APPLICATION_PARAMETER_ID = "getApplicationParameters(userId, compositeId, deviceId, applicationId, applicationParameterId)";

    /**
     * The query to retrieve the application parameters with the specified ID, application, device
     * profile, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_PROFILE_ID
     * @see QueryParameter#APPLICATION_ID
     * @see QueryParameter#APPLICATION_PARAMETER_ID
     */
    static final String APPLICATION_PARAMETERS_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_ID_APPLICATION_ID_APPLICATION_PARAMETER_ID = "getApplicationParameters(userId, compositeId, deviceProfileId, applicationId, applicationParameterId)";

    /**
     * The query to retrieve the application profiles with the specified user.
     * 
     * @see QueryParameter#USER_ID
     */
    static final String APPLICATION_PROFILES_FROM_USER_ID = "getApplicationProfiles(userId)";

    /**
     * The query to retrieve the application profiles with the specified ID and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#APPLICATION_PROFILE_ID
     */
    static final String APPLICATION_PROFILES_FROM_USER_ID_APPLICATION_PROFILE_ID = "getApplicationProfiles(userId, applicationProfileId)";

    /**
     * The query to retrieve the application profiles with the specified name and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#APPLICATION_PROFILE_NAME
     */
    static final String APPLICATION_PROFILES_FROM_USER_ID_APPLICATION_PROFILE_NAME = "getApplicationProfiles(userId, applicationProfileName)";

    /**
     * The query to retrieve the application profiles with the specified file name and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#FILE_NAME
     */
    static final String APPLICATION_PROFILES_FROM_USER_ID_FILE_NAME = "getApplicationProfiles(userId, fileName)";

    /**
     * The query to retrieve the application with the specified composite and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     */
    static final String APPLICATIONS_FROM_USER_ID_COMPOSITE_ID = "getApplications(userId, compositeId)";

    /**
     * The query to retrieve the application with the specified ID, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#APPLICATION_ID
     */
    static final String APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_APPLICATION_ID = "getApplications(userId, compositeId, applicationId)";

    /**
     * The query to retrieve the application with the specified name, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#APPLICATION_NAME
     */
    static final String APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_APPLICATION_NAME = "getApplications(userId, compositeId, applicationName)";

    /**
     * The query to retrieve the applications with the specified device, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_ID
     */
    static final String APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_ID = "getApplications(userId, compositeId, deviceId)";

    /**
     * The query to retrieve the applications with the specified ID, device, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_ID
     * @see QueryParameter#APPLICATION_ID
     */
    static final String APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_ID_APPLICATION_ID = "getApplications(userId, compositeId, deviceId, applicationId)";

    /**
     * The query to retrieve the applications with the specified name, device, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_ID
     * @see QueryParameter#APPLICATION_NAME
     */
    static final String APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_ID_APPLICATION_NAME = "getApplications(userId, compositeId, deviceId, applicationName)";

    /**
     * The query to retrieve the applications with the specified device profile, composite, and
     * user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_PROFILE_ID
     */
    static final String APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_ID = "getApplications(userId, compositeId, deviceProfileId)";

    /**
     * The query to retrieve the applications with the specified ID, device profile, composite, and
     * user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_PROFILE_ID
     * @see QueryParameter#APPLICATION_ID
     */
    static final String APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_ID_APPLICATION_ID = "getApplications(userId, compositeId, deviceProfileId, applicationId)";

    /**
     * The query to retrieve the applications with the specified name, device profile, composite,
     * and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_PROFILE_ID
     * @see QueryParameter#APPLICATION_NAME
     */
    static final String APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_ID_APPLICATION_NAME = "getApplications(userId, compositeId, deviceProfileId, applicationName)";

    /**
     * The query to retrieve the cell towers with the specified composite and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     */
    static final String CELL_TOWERS_FROM_USER_ID_COMPOSITE_ID = "getCellTowers(userId, compositeId)";

    /**
     * The query to retrieve the cell towers with the specified ID, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#CELL_TOWER_ID
     */
    static final String CELL_TOWERS_FROM_USER_ID_COMPOSITE_ID_CELL_TOWER_ID = "getCellTowers(userId, compositeId, cellTowerId)";

    /**
     * The query to retrieve the cellular configurations with the specified composite and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     */
    static final String CELLULAR_CONFIGURATIONS_FROM_USER_ID_COMPOSITE_ID = "getCellularConfigurations(userId, compositeId)";

    /**
     * The query to retrieve the commands with the specified execution.
     * 
     * @see QueryParameter#EXECUTION_ID
     */
    static final String COMMANDS_FROM_EXECUTION_ID = "getCommands(executionId)";

    /**
     * The query to retrieve the execution messages with the specified execution, composite, and
     * user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#EXECUTION_ID
     */
    static final String EXECUTION_MESSAGES_FROM_USER_ID_COMPOSITE_ID_EXECUTION_ID = "getExecutionMessages(userId, compositeId, executionId)";

    /**
     * The query to retrieve the composites with the specified user.
     * 
     * @see QueryParameter#USER_ID
     */
    static final String COMPOSITES_FROM_USER_ID = "getComposites(userId)";

    /**
     * The query to retrieve the composites with the specified ID and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     */
    static final String COMPOSITES_FROM_USER_ID_COMPOSITE_ID = "getComposites(userId, compositeId)";

    /**
     * The query to retrieve the composites with the specified name and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_NAME
     */
    static final String COMPOSITES_FROM_USER_ID_COMPOSITE_NAME = "getComposites(userId, compositeName)";

    /**
     * The query to delete the application logs with the specified executions.
     * 
     * @see QueryParameter#EXECUTION_LIST
     */
    static final String DELETE_APPLICATION_LOGS_FROM_EXECUTION_LIST = "deleteApplicationLogs(executionList)";

    /**
     * The query to delete the commands with the specified executions.
     * 
     * @see QueryParameter#EXECUTION_LIST
     */
    static final String DELETE_COMMANDS_FROM_EXECUTION_LIST = "deleteCommands(executionList)";

    /**
     * The query to delete the lane mappings with the specified simulations.
     * 
     * @see QueryParameter#SIMULATION_LIST
     */
    static final String DELETE_LANE_MAPPINGS_FROM_SIMULATION_LIST = "deleteLaneMappings(simulationList)";

    /**
     * The query to retreive the detectors with the specified simulation, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#SIMULATION_ID
     */
    static final String DETECTORS_FROM_USER_ID_COMPOSITE_ID_SIMULATION_ID = "getDetectors(userId, compositeId, simulationId)";

    /**
     * The query to retrieve the detectors with the specified ID, simulation, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#SIMULATION_ID
     * @see QueryParameter#DETECTOR_ID
     */
    static final String DETECTORS_FROM_USER_ID_COMPOSITE_ID_SIMULATION_ID_DETECTOR_ID = "getDetectors(userId, compositeId, simulationId, detectorId)";

    /**
     * The query to retrieve the device profiles with the specified composite and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     */
    static final String DEVICE_PROFILES_FROM_USER_ID_COMPOSITE_ID = "getDeviceProfiles(userId, compositeId)";

    /**
     * The query to retrieve the device profiles with the specified ID, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_PROFILE_ID
     */
    static final String DEVICE_PROFILES_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_ID = "getDeviceProfiles(userId, compositeId, deviceProfileId)";

    /**
     * The query to retrieve the device profiles with the specified name, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_PROFILE_NAME
     */
    static final String DEVICE_PROFILES_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_NAME = "getDeviceProfiles(userId, compositeId, deviceProfileName)";

    /**
     * The query to retrieve the devices with the specified composite and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     */
    static final String DEVICES_FROM_USER_ID_COMPOSITE_ID = "getDevices(userId, compositeId)";

    /**
     * The query to retrieve the devices with the specified ID, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_ID
     */
    static final String DEVICES_FROM_USER_ID_COMPOSITE_ID_DEVICE_ID = "getDevices(userId, compositeId, deviceId)";

    /**
     * The query to retrieve the devices with the specified name, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#DEVICE_NAME
     */
    static final String DEVICES_FROM_USER_ID_COMPOSITE_ID_DEVICE_NAME = "getDevices(userId, compositeId, deviceName)";

    /**
     * The query to retrieve the devices with the specified MAC address, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#MAC_ADDRESS
     */
    static final String DEVICES_FROM_USER_ID_COMPOSITE_ID_MAC_ADDRESS = "getDevices(userId, compositeId, macAddress)";

    /**
     * The query to retrieve the executions with the specified composite and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     */
    static final String EXECUTIONS_FROM_USER_ID_COMPOSITE_ID = "getExecutions(userId, compositeId)";

    /**
     * The query to retrieve the executions with the specified ID, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#EXECUTION_ID
     */
    static final String EXECUTIONS_FROM_USER_ID_COMPOSITE_ID_EXECUTION_ID = "getExecutions(userId, compositeId, executionId)";

    /**
     * The query to retrieve the executions with the specified name, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#EXECUTION_NAME
     */
    static final String EXECUTIONS_FROM_USER_ID_COMPOSITE_ID_EXECUTION_NAME = "getExecutions(userId, compositeId, executionName)";

    /**
     * The query to retrieve the executions with the specified status and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#EXECUTION_STATUS
     */
    static final String EXECUTIONS_FROM_USER_ID_EXECUTION_STATUS = "getExecutions(userId, executionStatus)";

    /**
     * The query to retrieve the file data with the specified simulation.
     * 
     * @see QueryParameter#SIMULATION_ID
     */
    static final String FILE_DATA_FROM_SIMULATION_ID = "getFileData(simulationId)";

    /**
     * The query to retrieve the lane manager data with the specified simulation, composite, and
     * user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#SIMULATION_ID
     */
    static final String LANE_MANAGER_DATA_FROM_USER_ID_COMPOSITE_ID_SIMULATION_ID = "getLaneManagerData(userId, compositeId, simulationId)";

    /**
     * The query to retrieve the lane mappings with the specified composite and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     */
    static final String LANE_MAPPINGS_FROM_USER_ID_COMPOSITE_ID = "getLaneMappings(userId, compositeId)";

    /**
     * The query to retrieve the lane mappings with the specified ID, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#LANE_MAPPING_ID
     */
    static final String LANE_MAPPINGS_FROM_USER_ID_COMPOSITE_ID_LANE_MAPPING_ID = "getLaneMappings(userId, compositeId, laneMappingId)";

    /**
     * The query to remove the file data from applications with the specified file name.
     * 
     * @see QueryParameter#FILE_NAME
     */
    static final String REMOVE_FILE_DATA_FROM_APPLICATIONS = "removeFileDataFromApplications(fileName)";

    /**
     * The query to retrieve the simulations with the specified composite and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     */
    static final String SIMULATIONS_FROM_USER_ID_COMPOSITE_ID = "getSimulations(userId, compositeId)";

    /**
     * The query to retrieve the simulations with the specified ID, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#SIMULATION_ID
     */
    static final String SIMULATIONS_FROM_USER_ID_COMPOSITE_ID_SIMULATION_ID = "getSimulations(userId, compositeId, simulationId)";

    /**
     * The query to retrieve the simulations with the specified name, composite, and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#SIMULATION_NAME
     */
    static final String SIMULATIONS_FROM_USER_ID_COMPOSITE_ID_SIMULATION_NAME = "getSimulations(userId, compositeId, simulationName)";

    /**
     * The query to retrieve the topography features with the specified composite and user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     */
    static final String TOPOGRAPHY_FEATURES_FROM_USER_ID_COMPOSITE_ID = "getTopographyFeatures(userId, compositeId)";

    /**
     * The query to retrieve the topography features with the specified feature ID, composite and
     * user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#FEATURE_ID
     */
    static final String TOPOGRAPHY_FEATURES_FROM_USER_ID_COMPOSITE_ID_FEATURE_ID = "getTopographyFeatures(userId, compositeId, featureId)";

    /**
     * The query to retrieve the topography features with the specified feature name composite and
     * user.
     * 
     * @see QueryParameter#USER_ID
     * @see QueryParameter#COMPOSITE_ID
     * @see QueryParameter#FEATURE_NAME
     */
    static final String TOPOGRAPHY_FEATURES_FROM_USER_ID_COMPOSITE_ID_FEATURE_NAME = "getTopographyFeatures(userId, compositeId, featureName)";

    /** The query to update stale executions when the server restarts. */
    static final String UPDATE_STALE_EXECUTIONS = "updateStaleExecutions()";

    /**
     * The query to retrieve the users with the specified email.
     * 
     * @see QueryParameter#EMAIL
     */
    static final String USERS_FROM_EMAIL = "getUsers(email)";

    /**
     * The query to retrieve the users with the specified username.
     * 
     * @see QueryParameter#USERNAME
     */
    static final String USERS_FROM_USERNAME = "getUsers(username)";

    /* prevents instantiation */
    private Query() {}
}