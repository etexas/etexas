package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas template simulation
 *
 * @author llaroussini
 */
public class TemplateSimulation extends Simulation {

    /**
     * Auto-generated serial UID
     */
    private static final long serialVersionUID = -8021833733583674000L;

    /**
     * Enumeration of templates
     *
     * @author llaroussini
     */
    public enum Template {

        /**
         * Example_01
         */
        EX_01("exam_01", "4X4 LIGHT TRAFFIC"),
        /**
         * Example_02
         */
        EX_02("exam_02", "4X4 LIGHT TRAFFIC"),
        /**
         * Example_03
         */
        EX_03("exam_03", "4X4 LIGHT TRAFFIC"),
        /**
         * Example_04
         */
        EX_04("exam_04", "4X4 LIGHT TRAFFIC"),
        /**
         * Example_05
         */
        EX_05("exam_05", "4X4 LIGHT TRAFFIC"),
        /**
         * Example_06
         */
        EX_06("exam_06", "4X4 LIGHT TRAFFIC"),
        /**
         * Example_07
         */
        EX_07("exam_07", "4X4 LIGHT TRAFFIC"),
        /**
         * Example_08
         */
        EX_08("exam_08", "5X4 HEAVY TRAFFIC"),
        /**
         * Example_09
         */
        EX_09("exam_09", "5X4 HEAVY TRAFFIC"),
        /**
         * Example_10
         */
        EX_10("exam_10", "5X4 HEAVY TRAFFIC"),
        /**
         * Example_11
         */
        EX_11("exam_11", "COMPACT DIAMOND"),
        /**
         * Example_12
         */
        EX_12("exam_12", "COMPACT DIAMOND"),
        /**
         * Example_13
         */
        EX_13("exam_13", "EX1 STANDARD DIAMOND"),
        /**
         * Example_14
         */
        EX_14("exam_14", "EX1 STANDARD DIAMOND"),
        /**
         * Example_15
         */
        EX_15("exam_15", "EX1 STANDARD DIAMOND"),
        /**
         * Example_16
         */
        EX_16("exam_16", "EX1 STANDARD DIAMOND"),
        /**
         * Example_17
         */
        EX_17("exam_17", "EX1 STANDARD DIAMOND"),
        /**
         * Example_18
         */
        //EX_18("exam_18", "STANDARD DIAMOND FREE U-TURNS."), TODO re-enable when working, Bug 8992
        /**
         * Example_19
         */
        EX_19("exam_19", "5X5 HEAVY TRAFFIC"),
        /**
         * Example_20
         */
        EX_20("exam_20", "5X5 HEAVY TRAFFIC");

        /**
         * The label of the template as it appears in the application
         */
        private String label;

        /**
         * The description of the template as it appears in the application
         */
        private String description;

        /**
         * Default constructor; sets the label and description
         *
         * @param label The string to set as the label
         * @param description The string to set as the description
         */
        Template(String label, String description) {
            this.label = label;
            this.description = description;
        }

        /**
         * Gets the label associated with the template as it is displayed in the
         * Web UI
         *
         * @return The label of the template
         */
        public String getLabel() {
            return this.label;
        }

        /**
         * Gets the description associated with the template as it is displayed
         * in the Web UI
         *
         * @return The description of the template
         */
        public String getDescription() {
            return this.description;
        }
    }

    /**
     * Default constructor. Sets Simulation Type to Template
     */
    public TemplateSimulation() {
        super();
        this.setSimFoundation(SimulationFoundation.TEMPLATE);
        this.entityType = ETexasEntityType.TEMPLATE_SIMULATION;
    }

    /**
     * The simulation template
     */
    private Template template;

    /**
     * The composite associated with the simulation
     */
    private UUID composite;

    /**
     * Gets the template
     *
     * @return the template
     */
    public Template getTemplate() {
        return this.template;
    }

    /**
     * Sets the template
     *
     * @param template the template to set
     */
    public void setTemplate(Template template) {
        this.template = template;
    }

    /**
     * Gets the composite UUID with which this simulation is associated
     *
     * @return the UUID of the composite with which this simulation is
     *         associated
     */
    public UUID getCompositeID() {
        return this.composite;
    }

    /**
     * Gets the composite with which this simulation is associated
     *
     * @return the composite with which this simulation is associated
     */
    public CompositeSimulation getComposite() {
        return ETexasEntityManager.getEntity(getCompositeID(), CompositeSimulation.class);
    }

    /**
     * Sets the composite for this simulation
     *
     * @param composite the composite to set
     */
    public void setComposite(CompositeSimulation composite) {
        UUID compositeUUID = composite.getUuid();
        ETexasEntityManager.addEntity(composite);
        this.composite = compositeUUID;
    }

}
