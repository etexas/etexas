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
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.webapp.datamodel.util.ICopyable;

/**
 * A traffic model simulation that can be repeatedly executed.
 * 
 * @author dranker
 * @author ablatt
 * @author emyers
 */
@Entity
@Table(name = "simulations")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Simulation extends AbstractEntity implements ICopyable<Simulation> {

    /** The serial version ID */
    private static final long serialVersionUID = 1L;

    /** The name of this simulation. */
    private String name;

    /** The type for this simulation. */
    @Enumerated(EnumType.STRING)
    private SimulationType type;

    /** The source of this simulation. */
    private String source;

    /** The x coordinate of this simulation */
    private double x;

    /** The y coordinate of this simulation */
    private double y;

    /** The width of this simulation. */
    @XmlTransient
    private double width;

    /** The height of this simulation. */
    @XmlTransient
    private double height;

    /** The step size for this simulation. */
    @XmlTransient
    @Column(name = "step_size")
    private double stepSize;

    /** The maximum number of steps in this simulation. */
    @XmlTransient
    @Column(name = "max_steps")
    private long maxSteps;

    /** The file data for this simulation. */
    @XmlTransient
    @JoinColumn(name = "file")
    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private FileData fileData = new FileData();

    /** The lane manager data for this simulation. */
    @XmlTransient
    @JoinColumn(name = "lane_manager")
    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private LaneManagerData laneManagerData = new LaneManagerData();

    /** The detectors for this simulation. */
    @XmlTransient
    @JoinColumn(name = "simulation")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Detector> detectors = new ArrayList<Detector>();

    /**
     * Returns the name of this simulation.
     * 
     * @return The string name of this simulation.
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of this simulation.
     * 
     * @param name The string name to set.
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the type for this simulation.
     * 
     * @return The type for this simulation.
     */
    public SimulationType getType() {

        return type;
    }

    /**
     * Sets the type for this simulation.
     * 
     * @param type The type to set.
     */
    public void setType(SimulationType type) {

        this.type = type;
    }

    /**
     * Returns the source of this simulation.
     * 
     * @return The string source of this simulation.
     */
    public String getSource() {

        return source;
    }

    /**
     * Sets the source of this simulation.
     * 
     * @param source The string source to set.
     */
    public void setSource(String source) {

        this.source = source;
    }

    /**
     * Getter for the x coordinate of the simulation.
     * 
     * @return The x coordinate of the simulation.
     */
    public double getX() {
        return x;
    }

    /**
     * Setter for the x coordinate of the simulation.
     * 
     * @param x The x coordinate of the simulation.
     */
    private void setX(double x) {
        this.x = x;
    }

    /**
     * Getter for the y coordinate of the simulation.
     * 
     * @return The y coordinate of the simulation.
     */
    public double getY() {
        return y;
    }

    /**
     * Setter for the y coordinate of the simulation.
     * 
     * @param y The y coordinate of the simulation.
     */
    private void setY(double y) {
        this.y = y;
    }

    /**
     * Sets the location of the simulation.
     * 
     * @param x The x coordinate of the simulation.
     * @param y The y coordinate of the simulation.
     */
    public void setLocation(double x, double y) {

        double deltaX = x - this.x;
        double deltaY = y - this.y;

        LaneManager lm = laneManagerData.getLaneManager();
        lm.shift(deltaX, deltaY);

        this.x = x;
        this.y = y;
    }

    /**
     * Returns the width of this simulation.
     * 
     * @return The double width of this simulation.
     */
    public double getWidth() {

        return width;
    }

    /**
     * Sets the width of this simulation.
     * 
     * @param width The double width to set.
     */
    public void setWidth(double width) {

        this.width = width;
    }

    /**
     * Returns the height of this simulation.
     * 
     * @return The double height of this simulation.
     */
    public double getHeight() {

        return height;
    }

    /**
     * Sets the height of this simulation.
     * 
     * @param height The double height to set.
     */
    public void setHeight(double height) {

        this.height = height;
    }

    /**
     * Returns the step size for this simulation.
     * 
     * @return The double step size for this simulation.
     */
    public double getStepSize() {

        return stepSize;
    }

    /**
     * Sets the step size for this simulation.
     * 
     * @param stepSize The double step size to set.
     */
    public void setStepSize(double stepSize) {

        this.stepSize = stepSize;
    }

    /**
     * Returns the maximum number of steps in this simulation.
     * 
     * @return The long maximum number of steps in this simulation.
     */
    public long getMaximumSteps() {

        return maxSteps;
    }

    /**
     * Sets the maximum number of steps in this simulation.
     * 
     * @param maxSteps The long maximum number of steps to set.
     */
    public void setMaximumSteps(long maxSteps) {

        this.maxSteps = maxSteps;
    }

    /**
     * Returns the file data for this simulation.
     * 
     * @return The file data for this simulation.
     */
    public FileData getFileData() {

        return fileData;
    }

    /**
     * Sets the file data for this simulation.
     * 
     * @param fileData The file data to set.
     */
    public void setFileData(FileData fileData) {

        this.fileData = fileData;
    }

    /**
     * Returns the lane manager data for this simulation.
     * 
     * @return The lane manager data for this simulation.
     */
    public LaneManagerData getLaneManagerData() {

        return laneManagerData;
    }

    /**
     * Sets the lane manager data for this simulation.
     * 
     * @param laneManagerData The lane manager data to set.
     */
    public void setLaneManagerData(LaneManagerData laneManagerData) {

        this.laneManagerData = laneManagerData;
    }

    /**
     * Returns the detectors for this simulation.
     * 
     * @return The list of detectors for this simulation.
     */
    public List<Detector> getDetectors() {

        return detectors;
    }

    /**
     * Sets the detectors for this simulation.
     * 
     * @param detectors The list of detectors to set.
     */
    public void setDetectors(List<Detector> detectors) {

        this.detectors = detectors;
    }

    /**
     * The method to copy this Simulation
     * 
     * @return The copied Simulation
     */
    @Override
    public Simulation copy() {
        Simulation simulation = new Simulation();
        simulation.setName(this.getName());
        simulation.setMaximumSteps(this.getMaximumSteps());
        simulation.setType(this.getType());
        simulation.setX(this.getX());
        simulation.setY(this.getY());
        simulation.setHeight(this.getHeight());
        simulation.setWidth(this.getWidth());
        simulation.setStepSize(this.getStepSize());
        simulation.setSource(this.getSource());

        FileData files = new FileData();
        files.setData(this.getFileData().getData());

        LaneManagerData laneManData = new LaneManagerData();
        laneManData.setLaneManager(this.getLaneManagerData().getLaneManager());

        simulation.setFileData(files);
        simulation.setLaneManagerData(laneManData);

        List<Detector> tempDetectors = this.getDetectors();
        List<Detector> detectors = new ArrayList<Detector>(tempDetectors.size());

        // add detectors to copy
        for (Detector tempdetector : tempDetectors) {
            Detector detector = tempdetector.copy();
            detectors.add(detector);
        }

        simulation.setDetectors(detectors);

        return simulation;
    }
}