package edg_ide.ui;

import com.intellij.openapi.project.Project;


// Needed separate from BlockVisualizerService because IntelliJ doesn't seem to like the Scala class.
public class BlockVisualizerServiceWrapper extends BlockVisualizerService {
  public BlockVisualizerServiceWrapper(Project project) {
    super(project);
  }
}
