package edg_ide.ui;

import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.components.StoragePathMacros;
import com.intellij.openapi.project.Project;


// PersistentStateComponent doesn't seem to like Scala classes, so here it is in Java.
class BlockVisualizerServiceState {
  public String panelBlockFile = "";
  public String panelBlockModule = "";
  public String panelBlockName = "";
  public float panelMainSplitterPos = 0.5f;
  public float panelBottomSplitterPos = 0.33f;
  public float panelLibrarySplitterPos = 0.5f;
  public int panelTabIndex = 0;
}


// Needed separate from BlockVisualizerService because IntelliJ doesn't seem to like the Scala class.
@State(name = "BlockVisualizerServiceWrapper", storages = @Storage(StoragePathMacros.WORKSPACE_FILE))
public class BlockVisualizerServiceWrapper extends BlockVisualizerService {
  public BlockVisualizerServiceWrapper(Project project) {
    super(project);
  }
}
