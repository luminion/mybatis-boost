package io.github.bootystar.mybatisplus.generate.config.core;

import com.baomidou.mybatisplus.generator.config.OutputFile;
import com.baomidou.mybatisplus.generator.config.builder.ConfigBuilder;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableField;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;
import io.github.bootystar.mybatisplus.util.ReflectHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.JdbcType;

import java.io.File;
import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 配置基类
 *
 * @author bootystar
 */
@Slf4j
public class CustomConfig extends BaseConfig {

    public CustomConfig(BaseConfig config) {
        ReflectHelper.copyFieldProperties(config, this);
    }

    public List<CustomFile> customFiles(ConfigBuilder config, TableInfo tableInfo) {
        List<CustomFile> customFiles = new ArrayList<>(8);
        String parentPath = config.getPathInfo().get(OutputFile.parent);
        String entityName = tableInfo.getEntityName();
        String pathUnderParent4DTO = package4DTO.replaceAll("\\.", "\\" + File.separator);
        String pathUnderParent4VO = package4VO.replaceAll("\\.", "\\" + File.separator);
        if (this.path4DTO == null) {
            this.path4DTO = parentPath + File.separator + pathUnderParent4DTO + File.separator;
        }
        if (this.path4VO == null) {
            this.path4VO = parentPath + File.separator + pathUnderParent4VO + File.separator;
        }

        if (generateInsert || generateImport) {
            String fileName = "InsertDTO.java";
            String path = path4DTO + File.separator + entityName + fileName;
            CustomFile.Builder builder = new CustomFile.Builder()
                    .fileName(fileName)
                    .filePath(path)
                    .templatePath("/templates/base/entityInsertDTO.java.vm")
                    .packageName(pathUnderParent4DTO);
            if (fileOverride) {
                builder.enableFileOverride();
            }
            customFiles.add(builder.build());
        }

        if (generateUpdate) {
            String fileName = "UpdateDTO.java";
            String path = path4DTO + File.separator + entityName + fileName;
            CustomFile.Builder builder = new CustomFile.Builder()
                    .fileName(fileName)
                    .filePath(path)
                    .templatePath("/templates/base/entityUpdateDTO.java.vm")
                    .packageName(pathUnderParent4DTO);
            if (fileOverride) {
                builder.enableFileOverride();
            }
            customFiles.add(builder.build());
        }

        if (selectDTO == null) {
            String fileName = "SelectDTO.java";
            String path = path4DTO + File.separator + entityName + fileName;
            CustomFile.Builder builder = new CustomFile.Builder()
                    .fileName(fileName)
                    .filePath(path)
                    .templatePath("/templates/base/entitySelectDTO.java.vm")
                    .packageName(pathUnderParent4DTO);
            if (fileOverride) {
                builder.enableFileOverride();
            }
            customFiles.add(builder.build());
        }

        String fileName = "VO.java";
        String path = path4VO + File.separator + entityName + fileName;
        CustomFile.Builder builder = new CustomFile.Builder()
                .fileName(fileName)
                .filePath(path)
                .templatePath("/templates/base/entityVO.java.vm")
                .packageName(pathUnderParent4VO);
        if (fileOverride) {
            builder.enableFileOverride();
        }
        customFiles.add(builder.build());
        return customFiles;
    }

    public Map<String, Object> renderData(TableInfo tableInfo) {

        // DTO及VO导入的包
        Set<String> importPackages = tableInfo.getImportPackages();
        Set<String> importPackages4DTO = new HashSet<>();
        for (String importPackage : importPackages) {
            if (!importPackage.startsWith("com.baomidou.mybatisplus.annotation")) {
                importPackages4DTO.add(importPackage);
            }
        }
        this.importPackages4DTO = importPackages4DTO;

        // 当前时间
        this.nowTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));

        // 时间类型列表
        // SQL Server 2008
        // JDBC 4.2 JDK8
        // JDBC 4.2 JDK8
        // 对应fields[i].metaInfo.jdbcType
        this.jdbcTimeTypes = Arrays.asList(
                JdbcType.DATE, JdbcType.TIME, JdbcType.TIMESTAMP, JdbcType.DATETIMEOFFSET,// SQL Server 2008
                JdbcType.TIME_WITH_TIMEZONE,// JDBC 4.2 JDK8
                JdbcType.TIMESTAMP_WITH_TIMEZONE // JDBC 4.2 JDK8
        );

        // 排序字段sql
        List<TableField> sortfields = tableInfo.getFields();
        List<String> existColumnNames = sortfields.stream().map(TableField::getColumnName).collect(Collectors.toList());
        if (orderColumnMap != null && !orderColumnMap.isEmpty()) {
            orderColumnMap.entrySet().stream()
                    .filter(e -> existColumnNames.contains(e.getKey()))
                    .map(e -> String.format("a.%s%s", e.getKey(), e.getValue() ? " DESC" : ""))
                    .reduce((e1, e2) -> e1 + " , " + e2)
                    .ifPresent(e -> this.orderBySql = e);
        }

        // swagger UUID相关
        if (this.swaggerAnnotationWithUUID) {
            this.swaggerUUID = "_" + UUID.randomUUID().toString().substring(0, 4).toUpperCase();
        }

        // 额外字段后缀
        LinkedHashMap<String, String> build = this.extraFieldSuffixBuilder.build();
        if (build != null && !build.isEmpty()) {
            this.overrideInitSuffixBuilder = true;
            this.extraFieldSuffixMap = build;
        }

        // 添加自定义配置字段信息
        HashMap<String, Object> data = new HashMap<>();
        try {
            Collection<Field> fields = MybatisPlusReflectHelper.fieldMap(getClass()).values();
            for (Field field : fields) {
                data.put(field.getName(), field.get(this));
            }
        } catch (IllegalAccessException e) {
            log.error("Generate Injection Field Error Please Report to Developer", e);
        }
        return data;
    }


}
