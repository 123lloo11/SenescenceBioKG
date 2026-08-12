# SenescenceBioKG ontology

The ontology defines 14 entity types: Study, MaterialPlatform, MaterialComposition, FabricationStrategy, DesignMode, Stimulus, ResponsiveBehavior, TherapeuticCargo, TargetCell, SenescenceEvidence, Mechanism, DiseaseModel, RegenerativeEndpoint, and ValidationStage. Ten types are instantiated as canonical nodes in v1.0.

MaterialPlatform is the source of the frozen material-centered relations:

| Relation | Target concept |
|---|---|
| `USES` | DesignMode |
| `COMPOSED_OF` | MaterialComposition |
| `FABRICATED_BY` | FabricationStrategy |
| `RESPONDS_TO` | Stimulus |
| `DELIVERS` | TherapeuticCargo |
| `RELEASES` | TherapeuticCargo or released bioactive component |
| `TARGETS` | TargetCell |
| `MODULATES` | Mechanism |
| `PROMOTES` | RegenerativeEndpoint |
| `VALIDATED_AT` | ValidationStage |
| `ACTIVATES` | Mechanism; defined but not instantiated in v1.0 |
| `SUPPRESSES` | Mechanism; defined but not instantiated in v1.0 |

`RESPONDS_TO` requires a defined stimulus that directly changes material or platform behavior, such as bond cleavage, degradation, swelling, phase change, deformation, signal output, permeability, mechanics, or release. ROS scavenging, antioxidant activity, ordinary sustained release, or residence in a high-ROS environment is insufficient by itself.

Validation stages are Cell, Ex vivo, Organoid/chip, Small Animal, Large Animal, Human-derived, and Clinical. Human-derived means evidence from human cells, tissues, or samples and is not Clinical. Clinical requires administration of the material intervention to human participants with reported outcomes.

Same-record relation co-support is a retrieval construct and does not establish causal mediation.
