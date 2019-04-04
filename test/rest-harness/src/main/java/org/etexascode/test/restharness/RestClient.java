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
package org.etexascode.test.restharness;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.ws.rs.ClientErrorException;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.etexascode.devicedata.LogData;
import org.etexascode.webapp.datamodel.DetectorData;
import org.etexascode.webapp.datamodel.ExecData;
import org.etexascode.webapp.datamodel.MessageType;
import org.etexascode.webapp.datamodel.ProjData;
import org.etexascode.webapp.datamodel.SimData;
import org.etexascode.webapp.datamodel.app.AppDefinition;
import org.etexascode.webapp.datamodel.app.AppDefinitionParam;
import org.etexascode.webapp.datamodel.app.AppInstance;
import org.etexascode.webapp.datamodel.app.RemoteAppDefinition;
import org.etexascode.webapp.datamodel.device.DeviceRule;
import org.etexascode.webapp.datamodel.device.EquipmentRule;
import org.etexascode.webapp.datamodel.device.PercentageRule;
import org.etexascode.webapp.datamodel.device.ReportRule;
import org.etexascode.webapp.datamodel.utils.WrappedList;
import org.etexascode.xmlclasses.SimulationTemplate;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.glassfish.jersey.media.multipart.FormDataBodyPart;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.MultiPartFeature;
import org.glassfish.jersey.media.multipart.file.FileDataBodyPart;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.jaxrs.json.JacksonJaxbJsonProvider;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;

/**
 * Tests for the REST simulation, project, apps, devices, and detectors API.
 * 
 * @author jrutherford
 * @author ablatt
 * @author bbadillo
 */
public class RestClient {

    /**
     * The client used to generate the calls.
     */
    private final Client c;

    /**
     * The resource pointing to the root of the REST calls.
     */
    private final WebTarget restTarget;

    /**
     * Internal string for specifying media type.
     */
    private final String mediaType = MediaType.APPLICATION_JSON;

    /**
     * The name of the user.
     */
    private final String user;

    public static void main(String[] args) {
        RestClient ths = new RestClient("localhost:8080", "test", "test");
        ths.testAllFunctions();
    }

    public RestClient(String hostLocation, String user, String pass) {
        this.user = user;

        c = ClientBuilder.newClient();
        c.register(new JacksonJaxbJsonProvider(new ObjectMapper().enable(MapperFeature.USE_WRAPPER_NAME_AS_PROPERTY_NAME).setAnnotationIntrospector(new JaxbAnnotationIntrospector()),
                JacksonJaxbJsonProvider.DEFAULT_ANNOTATIONS));
        c.register(MultiPartFeature.class);
        WebTarget webTarget = c.target("http://" + hostLocation + "/rest/");

        Form form = new Form();
        form.param("j_username", user);
        form.param("j_password", pass);

        String token = webTarget.path("user").path("login").request().post(Entity.form(form), String.class);
        restTarget = webTarget.register(HttpAuthenticationFeature.basic(user, token));
    }

    /**
     * Executes testing of all REST functions.
     */
    public void testAllFunctions() {
        String tag1 = "tag1";
        Long test1 = createSimFromTemplate("test1", "exam_01");
        Long test2 = createSimFromTemplate("test2", "exam_07");

        List<SimData> sims = getSims();

        List<Long> simIds = new ArrayList<Long>(sims.size());
        for (SimData sim : sims) {
            List<ExecData> execsBySim = getExecsBySim(sim.getId());
            simIds.add(sim.getId());
        }

        addTagToSims(tag1, simIds);
        List<SimData> simsByProject = getSimsByTag(tag1);
        List<ProjData> projects = getTags();
        System.out.println("Number of sims: " + sims.size());
        System.out.println("Number of sims in project: " + simsByProject.size());
        System.out.println("Number of projects : " + projects.size());
        removeTagFromSims(tag1, simIds);

        for (Long simId : simIds) {
            deleteSim(simId.toString());
        }

        sims = getSims();
        simsByProject = getSimsByTag(tag1);
        projects = getTags();
        System.out.println("Number of sims: " + sims.size());
        System.out.println("Number of sims in project: " + simsByProject.size());
        System.out.println("Number of projects : " + projects.size());

        List<AppDefinition> apps = getAppsList();
        System.out.println("Number of apps: " + apps.size());

        List<SimulationTemplate> templates = getTemplates();
        System.out.println("Number of templates: " + templates.size());
        for (SimulationTemplate tsc : templates) {
            System.out.println("Template Name: " + tsc.getName() + " Template Description: " +
                    tsc.getDescription());
        }
    }

    /**
     * Create a new simulation from a template.
     * 
     * @param name The simulation name.
     * @param template The name of the template to use.
     * @return Newly created simulation.
     */
    public Long createSimFromTemplate(String name, String template) {
        WebTarget resource = restTarget.path("api").path("sims");
        Form form = new Form();
        form.param("name", name);
        form.param("template", template);
        try {
            String resp = resource.request(mediaType).post(Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE), String.class);
            Long simId = new Long(0);
            try {
                JSONObject jsonResp = new JSONObject(resp);
                simId = jsonResp.getLong("simId");
            }
            catch (JSONException e) {
                e.printStackTrace();
            }
            return simId;
        }
        catch (ClientErrorException e) {
            System.out.println(e.getResponse().readEntity(String.class));
        }
        return null;
    }

    /**
     * Delete a simulation.
     * 
     * @param id The simulation id.
     * @return The simulation was deleted (true/false).
     */
    public boolean deleteSim(String id) {
        WebTarget resource = restTarget.path("api").path("sims").path(id);

        Response resp = resource.request().delete(Response.class);
        return resp.getStatus() == 204;
    }

    /**
     * Get all the simulations.
     * 
     * @return List of simulations.
     */
    public List<SimData> getSims() {
        WebTarget resource = restTarget.path("api").path("sims");

        WrappedList<SimData> resp = resource.request(mediaType).get(new GenericType<WrappedList<SimData>>() {});
        return resp.getList();
    }

    /**
     * Get simulations by project.
     *
     * @param tagName The name of tag to query.
     * @return List of simulations.
     */
    public List<SimData> getSimsByTag(String tagName) {
        WebTarget resource = restTarget.path("api").path("sims").queryParam("tagName", tagName);

        WrappedList<SimData> resp = resource.request(mediaType).get(new GenericType<WrappedList<SimData>>() {});
        return resp.getList();
    }

    /**
     * Get tags.
     *
     * @return A list of tags.
     */
    public List<ProjData> getTags() {
        WebTarget resource = restTarget.path("api").path("tags");

        WrappedList<ProjData> resp = resource.request(mediaType).get(new GenericType<WrappedList<ProjData>>() {});
        return resp.getList();
    }

    /**
     * Add tag to select simulations.
     *
     * @param tagName The tag to add simulations.
     * @param simIds The simulations to add to the project.
     * @return A list of simulation ids that were added.
     */
    public List<Long> addTagToSims(String tagName, List<Long> simIds) {
        WebTarget resource = restTarget.path("api").path("tags").path(tagName);

        Form form = new Form();
        for (Long simId : simIds) {
            form.param("simId", simId.toString());
        }
        WrappedList<Long> resp = resource.request(mediaType).put(Entity.form(form), new GenericType<WrappedList<Long>>() {});
        return resp.getList();
    }

    /**
     * Remove tag from select simulations.
     *
     * @param tagName The tag to remove simulations from.
     * @param simIds The simulations to remove from the project.
     * @return A list of simulation ids that were removed.
     */
    public List<Long> removeTagFromSims(String tagName, List<Long> simIds) {
        WebTarget resource = restTarget.path("api").path("tags").path(tagName);
        for (Long simId : simIds) {
            resource.queryParam("simId", simId.toString());
        }

        WrappedList<Long> resp = resource.request(mediaType).delete(new GenericType<WrappedList<Long>>() {});
        return resp.getList();
    }

    /**
     * Get executions by simulation.
     * 
     * @param simId The simulation id.
     * @return A list of executions for a particular simulation.
     */
    public List<ExecData> getExecsBySim(Long simId) {
        WebTarget resource = restTarget.path("api").path("sims").path(simId.toString()).path("execs");

        WrappedList<ExecData> resp = resource.request(mediaType).get(new GenericType<WrappedList<ExecData>>() {});
        return resp.getList();
    }

    /**
     * Get the list of apps this user owns.
     * 
     * @return A list of apps this user owns.
     */
    public List<AppDefinition> getAppsList() {
        WebTarget outgoing = restTarget.path("api").path("apps");
        WrappedList<AppDefinition> res = outgoing.request(mediaType).get(new GenericType<WrappedList<AppDefinition>>() {});
        return res.getList();
    }

    /**
     * Get a list of available templates.
     *
     * @return The list of templates.
     */
    public List<SimulationTemplate> getTemplates() {
        WebTarget outgoing = restTarget.path("api").path("templateSims");
        WrappedList<SimulationTemplate> res = outgoing.request(mediaType).get(new GenericType<WrappedList<SimulationTemplate>>() {});
        return res.getList();
    }

    /**
     * Delete an app jar.
     * 
     * @param jarName The name of the jar to delete.
     * @return SUCCESS/FAILURE
     */
    public boolean deleteAppJar(String jarName) {
        WebTarget outgoing = restTarget.path("api").path("apps").path("jar").path(jarName);
        Response response = outgoing.request(mediaType).delete(Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Delete a device.
     * 
     * @param simId The id of the sim the device is a part of.
     * @param deviceId The id of the device.
     * @return SUCCESS/FAILURE
     */
    public boolean deleteDevice(String simId, String deviceId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("devices").path(deviceId);
        Response response = outgoing.request(mediaType).delete(Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Remove an app from a device.
     * 
     * @param simId The id of the sim the device is a part of.
     * @param deviceId The id of the device to remove the app from.
     * @param appId The id of the app to remove.
     * @return SUCCESS/FAILURE
     */
    public boolean removeAppFromDevice(String simId, String deviceId, String appId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("devices").path(deviceId).path("apps").path(appId);
        Response response = outgoing.request(mediaType).delete(Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Delete a detector.
     * 
     * @param simId The id of the sim the detector is a part of.
     * @param detId The id of the detector to delete.
     * @return SUCCESS/FAILURE
     */
    public boolean deleteDetector(String simId, String detId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("detectors").path(detId);
        Response response = outgoing.request(mediaType).delete(Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Delete a remote app.
     * 
     * @param appId The id of the remote app to delete.
     * @return SuCCESS/FAILURE
     */
    public boolean deleteRemoteApp(String appId) {
        WebTarget outgoing = restTarget.path("api").path("apps").path("remote").path(appId);
        Response response = outgoing.request(mediaType).delete(Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Get a list of detectors associated with a specific sim.
     * 
     * @param simId The id of the sim the detectors are a part of.
     * @return A list of detectors that are a part of that sim.
     */
    public List<DetectorData> getDetectors(String simId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("detectors");
        WrappedList<DetectorData> ret = outgoing.request(mediaType).get(new GenericType<WrappedList<DetectorData>>() {});
        return ret.getList();
    }

    /**
     * Get a list of apps on a device.
     * 
     * @param simId The id of the sim config the device is a part of.
     * @param devId The id of the device.
     * @return The list of apps associated with the device.
     */
    public List<AppInstance> getAppsOnDevice(String simId, String devId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("devices").path(devId).path("apps");
        WrappedList<AppInstance> ret = outgoing.request(mediaType).get(new GenericType<WrappedList<AppInstance>>() {});
        return ret.getList();
    }

    /**
     * Get a list of devices for a sim config.
     * 
     * @param simId The id of the sim config.
     * @return The list of devices associated with that sim config.
     */
    public List<DeviceRule> getDevices(String simId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("devices");
        String s = outgoing.request(mediaType).get(String.class);
        JSONObject j;
        ObjectMapper om = new ObjectMapper();
        om.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

        JSONArray ja;
        List<DeviceRule> ret = null;
        try {
            j = new JSONObject(s);
            ja = j.getJSONArray((String)j.keys().next());
            ret = new ArrayList<DeviceRule>(ja.length());
            DeviceRule dr = null;
            for (int i = 0; i < ja.length(); i++) {
                dr = parseToType(om, ja.getJSONObject(i).toString(), PercentageRule.class);
                if (dr == null) {
                    dr = parseToType(om, ja.getJSONObject(i).toString(), ReportRule.class);
                }

                if (dr == null) {
                    dr = parseToType(om, ja.getJSONObject(i).toString(), EquipmentRule.class);
                }
                ret.add(dr);
            }
        }
        catch (JSONException e1) {
            throw new RuntimeException("Error Parsing Devices", e1);
        }

        return ret;
    }

    private <E> E parseToType(ObjectMapper om, String jsonObject, Class<E> type) {
        E ret = null;
        try {
            ret = (E)om.readValue(jsonObject, type);
        }
        catch (JsonParseException e) {}
        catch (JsonMappingException e) {}
        catch (IOException e) {}
        return ret;
    }

    /**
     * Start a new exec.
     * 
     * @param simId The id of the sim to use as the basis for the exec.
     * @return The exec's metadata.
     */
    public ExecData startExec(String simId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("execs");
        ExecData ed = outgoing.request().accept(mediaType).post(Entity.entity(null, MediaType.APPLICATION_FORM_URLENCODED), ExecData.class);
        return ed;
    }

    /**
     * Advance an execution.
     * 
     * @param simId The id of the sim which contains the execution to advance.
     * @param execId The id of the exec to advance.
     * @return The current sim time of the exec.
     */
    public String advanceExec(String simId, String execId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("execs").path(execId);
        String ret = outgoing.request(mediaType).put(Entity.entity(null, mediaType), String.class);
        return ret;
    }

    /**
     * Get the message currently available on a specific execution.
     * 
     * @param simId The id of the sim the execution is part of.
     * @param execId The id of the execution to get messages for.
     * @return The messages in stated execution.
     */
    public List<MessageType> getMessages(String simId, String execId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("execs").path(execId).path("getAllMessages");
        WrappedList<MessageType> ret = outgoing.request(mediaType).get(new GenericType<WrappedList<MessageType>>() {});
        return ret.getList();
    }

    /**
     * Add a remote app to the list of available Remote apps on the server.
     * 
     * @param appName The name to use for this Remote app.
     * @param appType The type of the app (available: AppDefData.APPTYPE_RSE, AppDefData.APPTYPE_OBU
     *        and AppDefData.APPTYPE_REPORT)
     * @param paramDefaults The paramters for this app.
     * @return The resulting remote app.
     */
    public List<AppDefinition> addRemoteApp(String appName, String appType, Map<String, String> paramDefaults) {
        RemoteAppDefinition app = new RemoteAppDefinition();
        app.setAppName(appName);
        app.setDevTarget(appType);
        List<AppDefinitionParam> appParams = new ArrayList<AppDefinitionParam>(paramDefaults.size());
        for (Entry<String, String> entry : paramDefaults.entrySet()) {
            AppDefinitionParam adp = new AppDefinitionParam();
            adp.setDescription("default");
            adp.setDisplayName(entry.getKey());
            adp.setName(entry.getKey());
            adp.setParam(entry.getValue());
            appParams.add(adp);
        }
        app.setAppParams(appParams);
        app.setRemovable(true);
        List<RemoteAppDefinition> ard = new ArrayList<RemoteAppDefinition>(1);
        ard.add(app);

        WebTarget outgoing = restTarget.path("api").path("apps").path("remote");
        try {
            WrappedList<AppDefinition> ret = outgoing.request(mediaType).post(Entity.entity(ard, mediaType), new GenericType<WrappedList<AppDefinition>>() {});
            return ret.getList();
        }
        catch (ClientErrorException e) {
            System.out.println(e.getResponse().readEntity(String.class));
        }
        return null;
    }

    /**
     * Add a detector to a sim config.
     * 
     * @param simId The id of the sim config to add the detector to.
     * @param laneId The id of the lane the detector is in.
     * @param width The width (cm) of the detector.
     * @param length The length (cm) of the detector.
     * @param dist The distance from the stop line (cm) the detector should be.
     * @return The data the server now has configured concerning the Detector.
     */
    public DetectorData addDetector(String simId, String laneId, String width, String length, String dist) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("detectors");
        Form form = new Form();
        form.param("laneNum", laneId);
        form.param("width", width);
        form.param("height", length);
        form.param("dist", dist);
        DetectorData ret = outgoing.request(mediaType).post(Entity.form(form), new GenericType<DetectorData>() {});
        return ret;
    }

    /**
     * Add a list of apps to a Device.
     * 
     * @param simId The id of the sim config this device is contained in.
     * @param devId The id of the device itself.
     * @param apps The apps to add to the device.
     * @return Data concerning those specific apps on those specific devices.
     */
    public List<AppInstance> addAppsToDevice(String simId, String devId, List<AppInstance> apps) {
        WrappedList<AppInstance> appList = new WrappedList<AppInstance>(apps);
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("devices").path(devId).path("apps");
        WrappedList<AppInstance> ret = outgoing.request(mediaType).post(Entity.entity(apps, mediaType), new GenericType<WrappedList<AppInstance>>() {});
        return ret.getList();
    }

    /**
     * Add an RSE device to the sim config.
     * 
     * @param simId The id of the sim config to add the RSE to.
     * @param name The name of the RSE.
     * @param x The x offset in the intersection the RSE should sit at.
     * @param y The y offset in the intersection the RSE should sit at.
     * @param z The z offset in the intersection the RSE should sit at.
     * @return The id of the RSE.
     */
    public String addRSE(String simId, String name, String x, String y, String z) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("devices").path("rse");
        Form form = new Form();
        form.param("name", name);
        form.param("xOffset", x);
        form.param("yOffset", y);
        form.param("zOffset", z);
        String ret = outgoing.request(mediaType).post(Entity.form(form), new GenericType<String>() {});
        return ret;
    }

    /**
     * Add obu percentage rules to a specific simulation.
     * 
     * @param simId The id of the sim config to the add the rule to.
     * @param name The name the rule should take.
     * @param percentage The percentage of vehicles which should have this rule.
     * @return The id of the rule.
     */
    public String addOBUPercentage(String simId, String name, String percentage) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("devices").path("obu").path("percentage");
        Form form = new Form();
        form.param("name", name);
        form.param("percentage", percentage);
        String ret = outgoing.request(mediaType).post(Entity.form(form), String.class);
        return ret;
    }

    /**
     * Add a specific jar file as a jar of apps.
     * 
     * @param f The jar file to add.
     * @param name The name to use for the jar file.
     */
    public void addJar(File f, String name) {
        try {
            FormDataMultiPart formDataMultiPart = new FormDataMultiPart();
            FileDataBodyPart fp = new FileDataBodyPart("archive-upload", f, MediaType.APPLICATION_OCTET_STREAM_TYPE);
            formDataMultiPart.bodyPart(fp);
            formDataMultiPart.bodyPart(new FormDataBodyPart("uname", user));
            formDataMultiPart.bodyPart(new FormDataBodyPart("name", name));
            formDataMultiPart.bodyPart(new FormDataBodyPart("form-type", "app-upload"));

            restTarget.path("upload").request().post(Entity.entity(formDataMultiPart, MediaType.MULTIPART_FORM_DATA_TYPE));
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    /**
     * Finishes an already started execution.
     * 
     * @param simId The id of the sim config the execution is under.
     * @param execId The id of the execution.
     * @return Success/Failure (true/false).
     */
    public boolean finishExec(String simId, String execId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("execs").path(execId).path("finish");
        Response response = outgoing.request(mediaType).put(Entity.entity(null, mediaType), Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Remove a jar of apps.
     * 
     * @param jarName The name of the jar to remove.
     * @return SUCCESS/FAILURE (true/false).
     */
    public boolean removeJar(String jarName) {
        WebTarget outgoing = restTarget.path("api").path("apps").path("jar").path(jarName);
        Response response = outgoing.request(mediaType).delete(Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Remove a specific execution.
     * 
     * @param simId The id of the sim config the exec belongs to.
     * @param execId The id of the exec to remove.
     * @return SUCCESS/FAILURE (true/false).
     */
    public boolean removeExec(String simId, String execId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("execs").path(execId);
        Response response = outgoing.request(mediaType).delete(Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Remove a specific sim config.
     * 
     * @param simId The id of the sim config the exec belongs to.
     * @return SUCCESS/FAILURE (true/false).
     */
    public boolean removeSimConfig(String simId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId);
        Response response = outgoing.request(mediaType).delete(Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Get a list of app logs.
     * 
     * @param simId The id of the simulation that owns the execution.
     * @param execId The id of the execution.
     * @param minTime The minimum time on the logs we should look for (0.0 if this should be
     *        ignored).
     * @param maxTime The maximum time on the logs we should look for (0.0 if this should be
     *        ignored).
     * @param keys A list of keys on the logs we should look for (empty if this should be ignored).
     * @param devIds A list of device ids on the logs we should look for (empty if this should be
     *        ignored).
     * @param appIds A list of app ids on the logs we should look for (empty if this should be
     *        ignored).
     * @return The list of logs matching the proposed query.
     */
    public List<LogData> getLogs(String simId, String execId, double minTime, double maxTime, List<String> keys, List<Long> devIds, List<String> appIds) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("execs").path(execId).path("appLogsAvailable");

        GenericType<String> tp = new GenericType<String>() {};
        JSONObject j = null;
        String s = null;
        try {
            s = outgoing.request(mediaType).get(tp);
            j = new JSONObject(s);
        }
        catch (JSONException e) {
            throw new RuntimeException("issue encountered when trying to check if logs are in the database", e);
        }
        try {
            while (!j.getBoolean("appLogsAvailable")) {
                try {
                    System.out.println("App logs are still being put into the data base. Waiting for 5 seconds.");
                    Thread.sleep(5000);// wait 5 seconds before next try
                    s = outgoing.request(mediaType).get(tp);
                    j = new JSONObject(s);
                }
                catch (InterruptedException e) {
                    throw new RuntimeException("thread interrupted for some reason");
                }
            }
        }
        catch (JSONException e) {
            throw new RuntimeException("issue encountered when trying to check if logs are in the database", e);
        }

        outgoing = restTarget.path("api").path("sims").path(simId).path("execs").path(execId).path("getAppLogTable");
        outgoing.queryParam("minTime", "" + minTime);
        outgoing.queryParam("maxTime", "" + maxTime);
        outgoing.queryParam("keys", JSONObject.valueToString(keys));
        outgoing.queryParam("devIds", JSONObject.valueToString(devIds));
        outgoing.queryParam("appIds", JSONObject.valueToString(appIds));
        WrappedList<LogData> ret = outgoing.request(mediaType).get(new GenericType<WrappedList<LogData>>() {});
        return ret.getList();
    }

    /**
     * Get the coordinate system of a simulation configuration.
     *
     * @param simId The id of the simulation configuration.
     * @return The coordinate system.
     */
    public String getCoordinateSystem(String simId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("config").path("sim").path("coordinate-system");
        String ret = outgoing.request().get(new GenericType<String>() {});
        return ret;
    }

    /**
     * Sets the coordinate system for the sim id.
     * 
     * @param simId The id of the sim config to set the coordinate system for.
     * @param coorSys The coordinate system to use.
     * @return SUCCESS/FAILURE (true/false)
     */
    public boolean setCoordinateSystem(String simId, String coorSys) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("config").path("sim").path("coordinate-system");
        Form form = new Form();
        form.param("coordinateSystem", coorSys);
        Response response = outgoing.request(mediaType).put(Entity.form(form), Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Get the communication model for the sim id.
     *
     * @param simId The id of the sim config.
     * @return The communication model of the sim config.
     */
    public String getCommunicationModel(String simId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("config").path("sim").path("communication-model");
        String ret = outgoing.request().get(new GenericType<String>() {});
        return ret;
    }

    /**
     * Set the communication model for the sim id.
     * 
     * @param simId The id of the sim config.
     * @param commModel The communication model the sim config should take.
     * @return SUCCESS/FAILURE (true/false)
     */
    public boolean setCommunicationModel(String simId, String commModel) {
        WebTarget outgoing = restTarget.path("api").path("sims").path("config").path("sim").path("communication-model");
        Form form = new Form();
        form.param("commModel", commModel);
        Response response = outgoing.request(mediaType).put(Entity.form(form), Response.class);
        return response.getStatus() == 204;
    }

    /**
     * Gets the center of the intersection (in terms of latitude and longitude).
     *
     * @param simId The id of the sim config.
     * @return [latitude, longitude]
     */
    public List<Double> getIntersectionCenter(String simId) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("config").path("sim").path("intersection-center");
        WrappedList<Double> ret = outgoing.request(mediaType).get(new GenericType<WrappedList<Double>>() {});
        List<Double> ret2 = ret.getList();
        if (ret2.size() != 2) {
            throw new RuntimeException("the return lat long return was not the expected size");
        }
        return ret2;
    }

    /**
     * Sets the center of the intersection (in terms of latitude and longitude).
     * 
     * @param simId The id of the sim config.
     * @param lat New latitude of the sim config.
     * @param lon New longitude of the sim config.
     * @return SUCCESS/FAILURE (true/false)
     */
    public boolean setIntersectionCenter(String simId, double lat, double lon) {
        WebTarget outgoing = restTarget.path("api").path("sims").path(simId).path("config").path("sim").path("intersection-center");
        Form form = new Form();
        form.param("latitude", Double.toString(lat));
        form.param("longitude", Double.toString(lon));
        Response response = outgoing.request(mediaType).put(Entity.form(form), Response.class);
        return response.getStatus() == 204;
    }
}
