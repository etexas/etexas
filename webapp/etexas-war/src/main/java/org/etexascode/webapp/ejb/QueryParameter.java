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

import javax.persistence.Parameter;

import org.etexascode.webapp.datamodel.ExecutionStatus;

/**
 * Provides query parameter constants.
 * 
 * @author emyers
 * @author ttevendale
 * @param <T> The query parameter type.
 */
class QueryParameter<T> implements Parameter<T> {

    /** The query parameter to specify an application ID value. */
    static final QueryParameter<Long> APPLICATION_ID = new QueryParameter<Long>(Long.class, "applicationId");

    /** The query parameter to specify an application name value. */
    static final QueryParameter<String> APPLICATION_NAME = new QueryParameter<String>(String.class, "applicationName");

    /** The query parameter to specify an application parameter ID value. */
    static final QueryParameter<Long> APPLICATION_PARAMETER_ID = new QueryParameter<Long>(Long.class, "applicationParameterId");

    /** The query parameter to specify an application profile ID value. */
    static final QueryParameter<Long> APPLICATION_PROFILE_ID = new QueryParameter<Long>(Long.class, "applicationProfileId");

    /** The query parameter to specify an application profile name value. */
    static final QueryParameter<String> APPLICATION_PROFILE_NAME = new QueryParameter<String>(String.class, "applicationProfileName");

    /** The query parameter to specify a cell tower ID value. */
    static final QueryParameter<Long> CELL_TOWER_ID = new QueryParameter<Long>(Long.class, "cellTowerId");

    /** The query parameter to specify a composite ID value. */
    static final QueryParameter<Long> COMPOSITE_ID = new QueryParameter<Long>(Long.class, "compositeId");

    /** The query parameter to specify a composite name value. */
    static final QueryParameter<String> COMPOSITE_NAME = new QueryParameter<String>(String.class, "compositeName");

    /** The query parameter to specify a detector ID value. */
    static final QueryParameter<Long> DETECTOR_ID = new QueryParameter<Long>(Long.class, "detectorId");

    /** The query parameter to specify a device ID value. */
    static final QueryParameter<Long> DEVICE_ID = new QueryParameter<Long>(Long.class, "deviceId");

    /** The query parameter to specify a device name value. */
    static final QueryParameter<String> DEVICE_NAME = new QueryParameter<String>(String.class, "deviceName");

    /** The query parameter to specify a device profile ID value. */
    static final QueryParameter<Long> DEVICE_PROFILE_ID = new QueryParameter<Long>(Long.class, "deviceProfileId");

    /** The query parameter to specify a device profile name value. */
    static final QueryParameter<String> DEVICE_PROFILE_NAME = new QueryParameter<String>(String.class, "deviceProfileName");

    /** The query parameter to specify an email address value. */
    static final QueryParameter<String> EMAIL = new QueryParameter<String>(String.class, "email");

    /** The query parameter to specify an execution ID value. */
    static final QueryParameter<Long> EXECUTION_ID = new QueryParameter<Long>(Long.class, "executionId");

    /** The query parameter to specify an execution name value. */
    static final QueryParameter<String> EXECUTION_NAME = new QueryParameter<String>(String.class, "executionName");

    /** The query parameter to specify an execution status value. */
    static final QueryParameter<ExecutionStatus> EXECUTION_STATUS = new QueryParameter<ExecutionStatus>(ExecutionStatus.class, "executionStatus");

    /** The query parameter to specify a feature ID value. */
    static final QueryParameter<Long> FEATURE_ID = new QueryParameter<Long>(Long.class, "featureId");

    /** The query parameter to specify a feature name value. */
    static final QueryParameter<String> FEATURE_NAME = new QueryParameter<String>(String.class, "featureName");

    /** The query parameter to specify a file name value. */
    static final QueryParameter<String> FILE_NAME = new QueryParameter<String>(String.class, "fileName");

    /** The query parameter to specify a lane mapping ID value. */
    static final QueryParameter<Long> LANE_MAPPING_ID = new QueryParameter<Long>(Long.class, "laneMappingId");

    /** The query parameter to specify a MAC address value. */
    static final QueryParameter<Long> MAC_ADDRESS = new QueryParameter<Long>(Long.class, "macAddress");

    /** The query parameter to specify a maximum simulation time value. */
    static final QueryParameter<Double> MAX_TIME = new QueryParameter<Double>(Double.class, "maxTime");

    /** The query parameter to specify a minimum simulation time value. */
    static final QueryParameter<Double> MIN_TIME = new QueryParameter<Double>(Double.class, "minTime");

    /** The query parameter to specify a parameter ID value. */
    static final QueryParameter<Long> PARAMETER_ID = new QueryParameter<Long>(Long.class, "parameterId");

    /** The query parameter to specify a simulation ID value. */
    static final QueryParameter<Long> SIMULATION_ID = new QueryParameter<Long>(Long.class, "simulationId");

    /** The query parameter to specify a simulation name value. */
    static final QueryParameter<String> SIMULATION_NAME = new QueryParameter<String>(String.class, "simulationName");

    /** The query parameter to specify a user ID value. */
    static final QueryParameter<Long> USER_ID = new QueryParameter<Long>(Long.class, "userId");

    /** The query parameter to specify a username value. */
    static final QueryParameter<String> USERNAME = new QueryParameter<String>(String.class, "username");

    /** The query parameter name to specify a list of application name values. */
    static final String APPLICATION_LIST = "applicationList";

    /** The query parameter name to specify a list of device ID values. */
    static final String DEVICE_LIST = "deviceList";

    /** The query parameter name to specify a list of execution ID values. */
    static final String EXECUTION_LIST = "executionList";

    /** The query parameter name to specify a list of key values. */
    static final String KEY_LIST = "keyList";

    /** The query parameter name to specify a list of simulation ID values. */
    static final String SIMULATION_LIST = "simulationList";

    /** The class type for this query parameter. */
    private Class<T> parameterType;

    /** The name of this query parameter. */
    private String name;

    /**
     * Constructs a new <code>QueryParameter</code> with the specified class type and name.
     * 
     * @param parameterType The class type to set.
     * @param name The string name to set.
     */
    private QueryParameter(Class<T> parameterType, String name) {

        this.parameterType = parameterType;
        this.name = name;
    }

    @Override
    public String getName() {

        return name;
    }

    @Override
    public Class<T> getParameterType() {

        return parameterType;
    }

    @Override
    public Integer getPosition() {

        return null;
    }
}