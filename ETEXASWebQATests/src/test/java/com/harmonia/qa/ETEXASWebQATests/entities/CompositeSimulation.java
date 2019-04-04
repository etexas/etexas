package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing a Composite Simulation
 *
 * @author llaroussini
 */
public class CompositeSimulation extends ETexasBaseEntity {

    /**
     * Auto-generated UID
     */
    private static final long serialVersionUID = -5783768499893728278L;

    /**
     * Default Constructor.
     */
    public CompositeSimulation() {
        super();
        this.entityType = ETexasEntityType.COMPOSITE_SIMULATION;
    }

    /**
     * The list of template simulations associated with this composite
     * simulation
     */
    private List<UUID> templateSims = new ArrayList<UUID>();

    /**
     * The list of uploaded simulations associated with this composite
     * simulation
     */
    private List<UUID> uploadedSims = new ArrayList<UUID>();

    /**
     * The name of the composite
     */
    private String name = "";

    /**
     * Gets the UUID list of template simulations for this composite
     *
     * @return the UUID template simulations list
     */
    public List<UUID> getTemplateSimIds() {
        return this.templateSims;
    }

    /**
     * Gets the composite's associated template simulations
     *
     * @return composite's associated template simulations
     */
    public List<TemplateSimulation> getTemplateSims() {
        List<TemplateSimulation> simList = new ArrayList<TemplateSimulation>(templateSims.size());
        for (UUID id : this.templateSims) {
            simList.add(ETexasEntityManager.getEntity(id, TemplateSimulation.class));
        }
        return simList;
    }

    /**
     * Sets the template simulation list for this composite
     *
     * @param templateSims -the list of template simulations to set
     */
    public void setTemplateSims(List<TemplateSimulation> templateSims) {
        List<UUID> simList = new ArrayList<UUID>(templateSims.size());
        for (TemplateSimulation simulation : templateSims) {
            UUID simUUID = simulation.getUuid();
            simList.add(simUUID);
        }
        this.templateSims = simList;
    }

    /**
     * Adds a template simulation to this composite
     *
     * @param templateSim the template simulation to add
     */
    public void addTemmplateSim(TemplateSimulation templateSim) {
        if (this.getTemplateSims() == null) {
            this.setTemplateSims(new ArrayList<TemplateSimulation>(1));
        }
        this.getTemplateSims().add(templateSim);
        if (templateSim.getComposite() == null || !templateSim.getComposite().equals(this)) {
            templateSim.setComposite(this);
        }
    }

    /**
     * Gets the UUID list of uploaded simulations for this composite
     *
     * @return the UUID uploaded simulations list
     */
    public List<UUID> getUploadedSimIds() {
        return this.uploadedSims;
    }

    /**
     * Gets the composite's associated uploaded simulations
     *
     * @return composite's associated uploaded simulations
     */
    public List<UploadedSimulation> getUploadedSims() {
        List<UploadedSimulation> simList = new ArrayList<UploadedSimulation>(uploadedSims.size());
        for (UUID id : this.uploadedSims) {
            simList.add(ETexasEntityManager.getEntity(id, UploadedSimulation.class));
        }
        return simList;
    }

    /**
     * Sets the uploaded simulation list for this composite
     *
     * @param uploadedSims -the list of uploaded simulations to set
     */
    public void setUploadedSims(List<UploadedSimulation> uploadedSims) {
        List<UUID> simList = new ArrayList<UUID>(uploadedSims.size());
        for (UploadedSimulation simulation : uploadedSims) {
            UUID simUUID = simulation.getUuid();
            simList.add(simUUID);
        }
        this.uploadedSims = simList;
    }

    /**
     * Adds an uploaded simulation to this composite
     *
     * @param uploadedSim the uploaded simulation to add
     */
    public void addUploadedSim(UploadedSimulation uploadedSim) {
        if (this.getUploadedSims() == null) {
            this.setUploadedSims(new ArrayList<UploadedSimulation>(1));
        }
        this.getUploadedSims().add(uploadedSim);
        if (uploadedSim.getComposite() == null || !uploadedSim.getComposite().equals(this)) {
            uploadedSim.setComposite(this);
        }
    }

    /**
     * Gets the composite name
     *
     * @return the composite name
     */
    public String getName() {
        if (this.name == null) {
            this.setName("");
        }
        return this.name;
    }

    /**
     * Sets the composite name
     *
     * @param name the composite name to set
     */
    public void setName(String name) {
        if (name == null) {
            this.name = "";
        }
        else {
            this.name = name;
        }
    }

}
