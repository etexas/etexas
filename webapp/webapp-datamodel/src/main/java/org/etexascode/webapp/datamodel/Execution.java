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
package org.etexascode.webapp.datamodel;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

/**
 * An execution of one or more traffic model simulations.
 * 
 * @author dranker
 * @author ttevendale
 * @author emyers
 */
@Entity
@Table(name = "executions")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Execution extends AbstractEntity {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The name of this execution. */
    private String name;

    /** The current step number for this execution. */
    @Column(name = "step_number")
    private long stepNumber;

    /** The maximum number of steps in this execution. */
    @Column(name = "max_steps")
    private long maxSteps;

    /** The step size for this execution. */
    @Column(name = "step_size")
    private double stepSize;

    /** The status of this execution. */
    @Enumerated(EnumType.STRING)
    private ExecutionStatus status;

    /** The execution messages. */
    @XmlTransient
    @JoinColumn(name = "execution")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<ExecutionMessage> executionMessages = new ArrayList<ExecutionMessage>();

    /**
     * Returns the name of this execution.
     * 
     * @return The string name of this execution.
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of this execution.
     * 
     * @param name The string name to set.
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the current step number for this execution.
     * 
     * @return The long current step number for this execution.
     */
    public long getStepNumber() {

        return stepNumber;
    }

    /**
     * Sets the current step number for this execution.
     * 
     * @param stepNumber The long current step number to set.
     */
    public void setStepNumber(long stepNumber) {

        this.stepNumber = stepNumber;
    }

    /**
     * Returns the maximum number of steps in this execution.
     * 
     * @return The long maximum number of steps in this execution.
     */
    public long getMaximumSteps() {

        return maxSteps;
    }

    /**
     * Sets the maximum number of steps in this execution.
     * 
     * @param maxSteps The long maximum number of steps to set.
     */
    public void setMaximumSteps(long maxSteps) {

        this.maxSteps = maxSteps;
    }

    /**
     * Returns the steps size for this execution.
     * 
     * @return The double step size for this execution.
     */
    public double getStepSize() {

        return stepSize;
    }

    /**
     * Sets the step size for this execution.
     * 
     * @param stepSize The double step size to set.
     */
    public void setStepSize(double stepSize) {

        this.stepSize = stepSize;
    }

    /**
     * Returns the status of this execution.
     * 
     * @return The execution status of this execution.
     */
    public ExecutionStatus getStatus() {

        return status;
    }

    /**
     * Sets the status of this execution.
     * 
     * @param status The execution status to set.
     */
    public void setStatus(ExecutionStatus status) {

        this.status = status;
    }

    /**
     * Returns the execution messages.
     * 
     * @return The list of execution messages.
     */
    public List<ExecutionMessage> getExecutionMessages() {

        return executionMessages;
    }

    /**
     * Adds the new execution messages.
     * 
     * @param messages The execution messages.
     */
    public void addExecutionMessages(List<ExecutionMessage> messages) {

        executionMessages.addAll(messages);
    }
}
