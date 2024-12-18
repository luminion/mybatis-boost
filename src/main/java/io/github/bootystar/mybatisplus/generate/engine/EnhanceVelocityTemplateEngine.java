package io.github.bootystar.mybatisplus.generate.engine;

import com.baomidou.mybatisplus.generator.config.builder.ConfigBuilder;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import com.baomidou.mybatisplus.generator.engine.AbstractTemplateEngine;
import com.baomidou.mybatisplus.generator.engine.VelocityTemplateEngine;
import io.github.bootystar.mybatisplus.generate.config.base.CustomConfig;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * 自定义Velocity引擎
 *
 * @author bootystar
 */
public class EnhanceVelocityTemplateEngine extends VelocityTemplateEngine {

    private final CustomConfig customConfig;

    public EnhanceVelocityTemplateEngine(CustomConfig customConfig) {
        this.customConfig = customConfig;
    }

    @Override
    protected void outputCustomFile(List<CustomFile> customFiles, TableInfo tableInfo, Map<String, Object> objectMap) {
        if (customFiles == null || customFiles.isEmpty()) {
            return;
        }
        customFiles.forEach(file -> {
            outputFile(new File(file.getFilePath()), objectMap, file.getTemplatePath(), file.isFileOverride());
        });
    }

    @Override
    public Map<String, Object> getObjectMap(ConfigBuilder config, TableInfo tableInfo) {
        Map<String, Object> objectMap = super.getObjectMap(config, tableInfo);
        if (customConfig != null) {
            Map<String, Object> customData = customConfig.renderData(tableInfo);
            if (customData != null) {
                objectMap.putAll(customData);
            }
        }
        objectMap.put("basePackage", config.getPackageConfig().getParent());
        return objectMap;
    }


    @Override
    public AbstractTemplateEngine batchOutput() {
        ConfigBuilder config = this.getConfigBuilder();
        List<TableInfo> tableInfoList = config.getTableInfoList();
        tableInfoList.forEach(tableInfo -> {
            Map<String, Object> objectMap = this.getObjectMap(config, tableInfo);
            Optional.ofNullable(config.getInjectionConfig()).ifPresent(t -> {
                // 添加自定义属性
                t.beforeOutputFile(tableInfo, objectMap);
                // 输出自定义文件
                outputCustomFile(t.getCustomFiles(), tableInfo, objectMap);
            });
            // entity
            outputEntity(tableInfo, objectMap);
            // mapper and xml
            outputMapper(tableInfo, objectMap);
            // service
            outputService(tableInfo, objectMap);
            // controller
            outputController(tableInfo, objectMap);
            // 自定义文件
            Optional.ofNullable(customConfig).ifPresent(t -> {
                // 输出自定义文件
//                String parentPath = getPathInfo(OutputFile.parent);
                outputCustomFile(customConfig.customFiles(config, tableInfo), tableInfo, objectMap);
            });
        });
        return this;
    }

}
