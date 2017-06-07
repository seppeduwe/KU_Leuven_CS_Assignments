Exercise sessions
Content

    MAS Exercise Session 3
    Attached Files:
        File pom.xml (1.102 KB)
        File AutoValue_UavRenderer_Builder.java (1.083 KB) 

    The exercise session of this week consists of two parts:
        Create a gradient field implementation to avoid collisions in a swarm of drones. Take a look at the UavExample to see how to configure a RoadModel based on a plane with collision detection (also explore the different visualization options). It is adviced to use the CommModel for communcation between agents.

        Note: the UAV related functionality is still under development (see the develop branch), therefore the code is not yet in a released version of RinSim. In order to run the example locally, you have to point to the 4.5.0-SNAPSHOT version. Take a look at the pom.xml attached to this message, to see how this can be done.
        Feedback on your project: you can ask questions related to your project objectives and your implementation.

    When copying the UavRenderer code to your own project, your IDE may start to complain about not being able to find AutoValue_UavRenderer_Builder. The reason for this message is that we use AutoValue. The @AutoValue annotation on a class indicates that that class is an immutable value object. AutoValue automatically generates much of the boiler plate code that is needed for implementing a value object (for details take a look at the project website). The way AutoValue works, is that it hooks itself into the compiler. Unfortunately, this has to be enabled in most IDE's, below are the instructions for Eclipse and IntelliJ for enabling AutoValue. In case you do not want to use AutoValue yourself, you can also choose to implement or copy the value object yourself (see AutoValue_UavRenderer_Builder.java in attachment for an example).

    pom.xml

    Add the following in your pom:

         <  dependency>   <  groupId>com.google.auto.value</  groupId>   <  artifactId>auto-value</  artifactId>   <  version>1.2</  version>   <  scope>provided</  scope> </  dependency> 

    (or copy it from https://github.com/google/auto/blob/master/value/userguide/index.md)

    Eclipse

    In the project properties, click on Java Compiler -> Annotation Processing
        Make sure that the following checkboxes are selected:
        - Enable annotation processing
        - Enable processing in editor
        In the text field labeled Generated source directory: the following path should be written:
        target/generated-sources/annotations

    Once that is done, unfold Annotation Processing (press the arrow) and click on Factory Path.
        In this view click on Add Variable... on the right side.
        Select M2_REPO
        Press OK
        Click on Edit...
        Paste the following text: M2_REPO/com/google/auto/value/auto-value/1.x/auto-value-1.x.jar
        Make sure to replace x with the version number of AutoValue that you are using.
        Press OK
        Press Apply

    IntelliJ

    Open Preference -> Build, Execution, Deployment
        Select your project and then tick Enable annotation processing
        On the right, tick Module content root
        In Production sources directory: replace the default text by generated (which tell IntelliJ to generate autovalue classes in a folder called generated)
        In Test sources directory: replace default text by generated_tests
        Press apply -> press OK
        Build the project once. You will see all the autovalue classes in the generated folder.
        Mark the generated folder as source (right click on the folder -> Mark directory as -> Sources Root)
    MAS Exercise Session 2

    Create a contract net protocol implementation for a pickup and delivery problem. For this exercise only the task allocation problem is relevant, e.g. what vehicle is delivering what package.
        use the PlaneRoadModel as road (as used in SimpleExample) so that vehicle routing becomes simple
        use the DefaultPDPModel for the pickup and delivery problem (as used in TaxiExample)
        use the CommModel for communication (as used in CommExample)

    MAS Exercise Session 1

    Part 1: 
        Goto https://github.com/rinde/RinSim
        Read the overview of the simulator 
        Follow the installation instructions and run SimpleExample
        Try to answer this question: why do the dots show this strange behavior, can you explain why the code results in this behavior?
        Copy the contents of SimpleExample.class (you can double click on this file in the example jar in your Maven Dependencies) to a file in your own project (as created via the instructions)
        If Eclipse shows some warnings make sure to read the troubleshooting tips: https://github.com/rinde/RinSim/blob/master/docs/howtorun.md#troubleshooting 
        Exercise: adapt the code such that the dots move in a straight line to their destinations instead of the random movement that they currently show. Make sure that when the dots reach their destination they pick a new random destination.

    Part 2: 
        Run the WarehouseExample from the jar. 
        Take a look at the code in WarehouseExample.java and AGVAgent.java, copy it to your own project.
        change the line: RoadModelBuilders.dynamicGraph(GraphCreator.createSimpleGraph())
                             to: RoadModelBuilders.dynamicGraph(GraphCreator.createGraph())
        Observe the exception that is thrown.
        Exercise: create an reactive agent that uses only local information (information from the model about the agent surroundings) to avoid deadlocks and if possible grid locks.
        Tips:
        - take a look at the visualization options of WarehouseRenderer and AGVRenderer
        - if you are confused about some of the behavior, remember that the underlying model of the AGVs is a graph where each AGV is represented as a simple Point. You can use the GraphRoadModelRenderer to show a basic graph.
        - some useful methods in the road model are isOccupied(Point) and hasRoadUserOn(Point,Point), also, catching the DeadlockException may be useful.

