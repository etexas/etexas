/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
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
package org.etexascode.webapp.cdi;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;
import javax.enterprise.context.ApplicationScoped;

import org.etexascode.webapp.ejb.ExecutionRunner;

/**
 * Maps the ID of running executions to their corresponding execution runner. Mappings are stored
 * for the total collection of running executions across all users, with a concurrent hash map
 * handling any threading issues.
 * 
 * @author bbadillo
 * @author jrutherford
 * @author emyers
 */
@ApplicationScoped
public class ExecutionMap {

    /** The map of executions and their corresponding execution runners. */
    private Map<Long, ExecutionRunner> executionMap;

    /** Initializes the map of executions. */
    @PostConstruct
    public void initialize() {

        executionMap = new ConcurrentHashMap<Long, ExecutionRunner>();
    }

    /**
     * Returns the execution runner for the specified execution.
     * 
     * @param executionId The long ID of the execution.
     * @return The execution runner for the specified execution.
     */
    public ExecutionRunner get(Long executionId) {

        return executionMap.get(executionId);
    }

    /**
     * Maps the specified execution to the specified execution runner.
     * 
     * @param executionId The long ID of the execution.
     * @param executionRunner The execution runner to associate with the execution.
     * @return The execution runner previously associated with the execution.
     */
    public ExecutionRunner put(Long executionId, ExecutionRunner executionRunner) {

        return executionMap.put(executionId, executionRunner);
    }

    /**
     * Removes the mapping for the specified execution.
     * 
     * @param executionId The long ID of the execution.
     * @return The execution runner previously associated with the execution.
     */
    public ExecutionRunner remove(Long executionId) {

        return executionMap.remove(executionId);
    }
}
