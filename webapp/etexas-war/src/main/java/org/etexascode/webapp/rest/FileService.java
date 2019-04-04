/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
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
package org.etexascode.webapp.rest;

import javax.inject.Inject;
import javax.interceptor.Interceptors;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.etexascode.webapp.datamodel.Composite;
import org.etexascode.webapp.datamodel.Simulation;
import org.etexascode.webapp.ejb.CompositeManager;
import org.etexascode.webapp.ejb.SimulationManager;
import org.etexascode.webapp.exception.WebAppException;

/**
 * The REST service for file operation requests.
 * 
 * @author bbadillo
 * @author jrutherford
 * @author emyers
 */
@Path("/files")
@Interceptors({ StringTrimmer.class })
public class FileService {

    /** The composite transaction manager. */
    @Inject
    private CompositeManager compositeManager;

    /** The simulation transaction manager. */
    @Inject
    private SimulationManager simulationManager;

    /**
     * Returns the file data for the specified composite.
     * 
     * @param compositeId The long ID of the composite.
     * @return A response (200) with the file data for the specified composite.
     * @throws WebAppException If the file data cannot be retrieved.
     */
    @GET
    @Path("/composites/{compositeId}")
    @Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_OCTET_STREAM })
    public Response getFileData(@PathParam("compositeId") final Long compositeId) throws WebAppException {

        byte[] data = compositeManager.getFileData(compositeId);
        Composite composite = compositeManager.getComposite(compositeId);
        return Response.ok(data, MediaType.APPLICATION_OCTET_STREAM).header("Content-Disposition", String.format("attachment;filename=\"%s.zip\"", composite.getName())).build();
    }

    /**
     * Returns the file data for the specified simulation.
     * 
     * @param simulationId The long ID of the simulation.
     * @return A response (200) with the file data for the specified simulation.
     * @throws WebAppException If the simulation files cannot be retrieved.
     */
    @GET
    @Path("/simulations/{simulationId}")
    @Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_OCTET_STREAM })
    public Response getSimulationFiles(@PathParam("simulationId") final Long simulationId) throws WebAppException {

        byte[] data = simulationManager.getFileData(simulationId);
        Simulation simulation = simulationManager.getSimulation(simulationId);
        return Response.ok(data, MediaType.APPLICATION_OCTET_STREAM).header("Content-Disposition", String.format("attachment;filename=\"%s.zip\"", simulation.getName())).build();
    }

    /**
     * Sets the file data for the simulation with the specified ID.
     * 
     * @param simulationId The long ID of the simulation
     * @param data The bytes of file data to set.
     * @return A response (204) with no additional content.
     * @throws WebAppException If the file data cannot be updated.
     */
    @PUT
    @Path("/simulations/{simulationId}")
    @Consumes({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_OCTET_STREAM })
    public Response setSimulationFiles(@PathParam("simulationId") final Long simulationId, byte[] data) throws WebAppException {

        simulationManager.updateFileData(simulationId, data);
        return Response.noContent().build();
    }
}