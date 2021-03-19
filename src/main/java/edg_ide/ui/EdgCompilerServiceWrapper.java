package edg_ide.ui;

import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.components.StoragePathMacros;
import com.intellij.openapi.project.Project;


// PersistentStateComponent doesn't seem to like Scala classes, so here it is in Java.
class EdgCompilerServiceState {
  public String serializedBlocks = "";
}


// Needed separate from the Scala superclass because IntelliJ doesn't seem to like Scala generated code
@State(name = "EdgCompilerServiceWrapper", storages = @Storage(StoragePathMacros.WORKSPACE_FILE))
public class EdgCompilerServiceWrapper extends EdgCompilerService {
  public EdgCompilerServiceWrapper(Project project) {
    super(project);
  }
}
