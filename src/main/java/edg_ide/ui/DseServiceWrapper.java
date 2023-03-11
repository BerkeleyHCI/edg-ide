package edg_ide.ui;

import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.components.StoragePathMacros;
import com.intellij.openapi.project.Project;


// PersistentStateComponent doesn't seem to like Scala classes, so here it is in Java.
class DseServiceState {
  // DSE Panel
  public int dseTabIndex = 0;
}


// Needed separate from DseService because IntelliJ doesn't seem to like the Scala class.
@State(name = "DseServiceWrapper", storages = @Storage(StoragePathMacros.WORKSPACE_FILE))
public class DseServiceWrapper extends DseService {
  public DseServiceWrapper(Project project) {
    super(project);
  }
}
