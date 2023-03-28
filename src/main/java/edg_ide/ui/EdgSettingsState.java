package edg_ide.ui;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;


@State(
        name = "EdgSettingsState",
        storages = @Storage("EdgSettingsPlugin.xml")
)
public class EdgSettingsState implements PersistentStateComponent<EdgSettingsState> {
    public String[] kicadDirectories = {};
    public boolean persistBlockCache = false;
    public boolean showProvenStatus = false;
    public boolean showInternalBlocks = false;
    public boolean showIdeErrors = false;


    public static EdgSettingsState getInstance() {
        return ApplicationManager.getApplication().getService(EdgSettingsState.class);
    }

    @Override
    public @Nullable EdgSettingsState getState() {
        return this;
    }

    @Override
    public void loadState(@NotNull EdgSettingsState state) {
        XmlSerializerUtil.copyBean(state, this);
    }
}
