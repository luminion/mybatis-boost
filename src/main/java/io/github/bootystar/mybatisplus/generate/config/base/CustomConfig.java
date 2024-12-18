package io.github.bootystar.mybatisplus.generate.config.base;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.generator.config.OutputFile;
import com.baomidou.mybatisplus.generator.config.builder.ConfigBuilder;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableField;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhance.enums.SqlExtraSuffix;
import io.github.bootystar.mybatisplus.generate.info.ClassInfo;
import io.github.bootystar.mybatisplus.generate.info.MethodInfo;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;
import lombok.SneakyThrows;
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
public abstract class CustomConfig {

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

        if ((generateSelect || generateExport) && selectDTO == null) {
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
//        if (generateExport && !exportOnVO) {
//            String fileName = "ExportDTO.java";
//            String path = path4DTO + File.separator + entityName + fileName;
//            CustomFile.Builder builder = new CustomFile.Builder()
//                    .fileName(fileName)
//                    .filePath(path)
//                    .templatePath("/templates/base/entityExportDTO.java.vm")
//                    .packageName(pathUnderParent4DTO);
//            if (fileOverride) {
//                builder.enableFileOverride();
//            }
//            customFiles.add(builder.build());
//
//        }
//        if (generateImport && !importOnVO) {
//            String fileName = "ImportDTO.java";
//            String path = path4DTO + File.separator + entityName + fileName;
//            CustomFile.Builder builder = new CustomFile.Builder()
//                    .fileName(fileName)
//                    .filePath(path)
//                    .templatePath("/templates/base/entityImportDTO.java.vm")
//                    .packageName(pathUnderParent4DTO);
//            if (fileOverride) {
//                builder.enableFileOverride();
//            }
//            customFiles.add(builder.build());
//        }

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
        HashMap<String, Object> data = new HashMap<>();
        // 额外字段后缀
        LinkedHashMap<String, String> build = this.extraFieldSuffixBuilder.build();
        if (build != null && !build.isEmpty()) {
            this.extraFieldSuffixCustom = true;
            this.extraFieldSuffixMap = build;
        }

        // 添加自定义字段
        try {
            Collection<Field> fields = MybatisPlusReflectHelper.fieldMap(getClass()).values();
            for (Field field : fields) {
                data.put(field.getName(), field.get(this));
            }
        } catch (IllegalAccessException e) {
            log.error("Generate Injection Field Error Please Report to Developer", e);
        }
        Set<String> importPackages = tableInfo.getImportPackages();
        Set<String> importPackages4DTO = new HashSet<>();
        for (String importPackage : importPackages) {
            if (!importPackage.startsWith("com.baomidou.mybatisplus.annotation")) {
                importPackages4DTO.add(importPackage);
            }
        }
        data.put("importPackages4DTO", importPackages4DTO);
        // 当前时间
        data.put("nowTime", LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));

        // 时间类型列表
        List<JdbcType> jdbcTimeTypes = Arrays.asList(JdbcType.DATE, JdbcType.TIME, JdbcType.TIMESTAMP, JdbcType.DATETIMEOFFSET,// SQL Server 2008
                JdbcType.TIME_WITH_TIMEZONE,// JDBC 4.2 JDK8
                JdbcType.TIMESTAMP_WITH_TIMEZONE // JDBC 4.2 JDK8
        );
        // 对应fields[i].metaInfo.jdbcType
        data.put("jdbcTimeTypes", jdbcTimeTypes);
        // 排序字段
        List<TableField> fields = tableInfo.getFields();
        List<String> existColumnNames = fields.stream().map(TableField::getColumnName).collect(Collectors.toList());
        if (orderColumnMap != null && !orderColumnMap.isEmpty()) {
            orderColumnMap.entrySet().stream()
                    .filter(e -> existColumnNames.contains(e.getKey()))
                    .map(e -> String.format("a.%s%s", e.getKey(), e.getValue() ? " DESC" : ""))
                    .reduce((e1, e2) -> e1 + " , " + e2)
                    .ifPresent(e -> data.put("orderBySql", e));
        }

        if (this.swaggerAnnotationWithUUID) {
            String uuid = "_" + UUID.randomUUID().toString().substring(0, 4).toUpperCase();
            data.put("swaggerUUID", uuid);
        } else {
            data.remove("swaggerUUID");
        }

        return data;
    }

    /**
     * 是否覆盖已有文件
     */
    protected boolean fileOverride;

    /**
     * 生成重写的父类方法
     */
    protected boolean overrideMethods = true;

    /**
     * swagger实体是否添加注解
     */
    protected boolean swaggerModelWithAnnotation;

    /**
     * swagger注解添加uuid标识
     */
    protected boolean swaggerAnnotationWithUUID;

    //------------------额外类相关配置----------------

    /**
     * mapper入参dto
     */
    protected ClassInfo mapperDTO;

    /**
     * 额外字段后缀
     */
    protected Map<String, String> extraFieldSuffixMap = SqlExtraSuffix.DEFAULT_MAP;

    /**
     * 额外字段后缀构建器
     */
    protected FieldSuffixBuilder extraFieldSuffixBuilder = new FieldSuffixBuilder();

    /**
     * 是否自定义后缀
     */
    protected boolean extraFieldSuffixCustom;

    /**
     * 实体查询dto
     */
    protected ClassInfo selectDTO;

    /**
     * DTO所在包
     */
    protected String package4DTO = "dto";

    /**
     * DTO文件的生成路径
     */
    protected String path4DTO;

    /**
     * VO所在包
     */
    protected String package4VO = "vo";

    /**
     * VO文件的生成路径
     */
    protected String path4VO;

    /**
     * 新增或修改时排除的字段
     */
    protected Collection<String> editExcludeColumns;

    // ------------------controller相关配置----------------

    /**
     * 返回结果方法
     */
    protected MethodInfo returnMethod = new MethodInfo();

    /**
     * 分页结果方法
     */
    protected MethodInfo pageMethod = new MethodInfo();

    /**
     * 基本服务注解
     */
    protected boolean autoWired;

    /**
     * controller是否使用@RequestBody注解
     */
    protected boolean requestBody = true;

    /**
     * servlet api包
     */
    protected String javaApiPackage = "javax";

    /**
     * 参数校验注解
     */
    protected boolean validated = true;
    /**
     * 跨域注解
     */
    protected boolean crossOrigins;

    /**
     * 复杂查询使用post请求
     */
    protected boolean postQuery = true;

    /**
     * 请求基础url
     */
    protected String baseUrl;

    /**
     * restful样式
     */
    protected boolean restful = true;


    // ------------------mapper相关配置----------------

    /**
     * 排序字段map
     * 字段名 -> 是否倒序
     */
    protected Map<String, Boolean> orderColumnMap = new LinkedHashMap<>();

    // ------------------ 生成相关配置----------------

    /**
     * 新增DTO
     */
    protected boolean generateInsert = true;
    /**
     * 更新DTO
     */
    protected boolean generateUpdate = true;
    /**
     * 生成删除方法
     */
    protected boolean generateDelete = true;
    /**
     * 查询DTO
     */
    protected boolean generateSelect = true;
    /**
     * 导入DTO
     */
    protected boolean generateImport = true;
    /**
     * 导出DTO
     */
    protected boolean generateExport = true;

    /**
     * 构建器
     *
     * @author bootystar
     */
    @SuppressWarnings("unused")
    public static abstract class Builder<C extends CustomConfig, B extends Builder<C, B>> {

        private final C config;

        public Builder(C config) {
            this.config = config;
        }

        /**
         * 获取配置
         *
         * @return {@link C }
         */
        protected C getConfig() {
            return this.config;
        }

        /**
         * 获取构建器
         *
         * @return {@link B }
         */
        @SuppressWarnings("unchecked")
        protected B getBuilder() {
            return (B) this;
        }


        /**
         * 构建模板配置对象
         *
         * @return 模板配置对象
         * @author bootystar
         */
        public C build() {
            return this.getConfig();
        }

        /**
         * 开启文件覆盖(vo及dto)
         *
         * @return {@link B }
         * @author bootystar
         */
        public B enableFileOverride() {
            this.getConfig().fileOverride = true;
            return this.getBuilder();
        }


        /**
         * 禁用swagger/springdoc模型实体的注解
         * <p>
         * 已知swagger注解在同名时有冲突, 禁用后请确保表注释不为空且不同名
         *
         * @return {@link B }
         * @author bootystar
         */
        public B enableSwaggerModelWithAnnotation() {
            this.getConfig().swaggerModelWithAnnotation = true;
            return this.getBuilder();
        }

        /**
         * 禁用swagger/springdoc文档额外uuid标识
         * <p>
         * 已知swagger注解在同名时有冲突, 禁用后请确保表注释不为空且不同名
         *
         * @return {@link B }
         * @author bootystar
         */
        public B enableSwaggerAnnotationWithUUID() {
            this.getConfig().swaggerAnnotationWithUUID = true;
            return this.getBuilder();
        }

        //==================DTO VO=======================

        /**
         * 使用指定类作为查询DTO
         *
         * @return {@link B }
         * @author bootystar
         */
        public B class4SelectDTO(Class<?> clazz) {
            this.getConfig().selectDTO = new ClassInfo(clazz);
            return this.getBuilder();
        }

        /**
         * DTO所在包
         *
         * @param packageName 包名
         * @return this
         * @author bootystar
         */
        public B package4DTO(String packageName) {
            this.getConfig().package4DTO = packageName;
            return this.getBuilder();
        }

        /**
         * DTO文件输出路径
         *
         * @param path 路径
         * @return {@link B }
         * @author bootystar
         */
        @SneakyThrows
        public B path4DTO(String path) {
            this.getConfig().path4DTO = new File(path).getCanonicalPath();
            return this.getBuilder();
        }

        /**
         * VO所在包
         *
         * @param packageName 包名
         * @return this
         * @author bootystar
         */
        public B package4VO(String packageName) {
            this.getConfig().package4VO = packageName;
            return this.getBuilder();
        }

        /**
         * DTO文件输出路径
         *
         * @param path 路径
         * @return {@link B }
         * @author bootystar
         */
        @SneakyThrows
        public B path4VO(String path) {
            this.getConfig().path4VO = new File(path).getCanonicalPath();
            return this.getBuilder();
        }

        /**
         * 新增或修改时排除的字段
         *
         * @param columns 字段名称
         * @return this
         * @author bootystar
         */
        public B editExcludeColumns(String... columns) {
            this.getConfig().editExcludeColumns = Arrays.asList(columns);
            return this.getBuilder();
        }

        //==================controller=======================

        /**
         * controller请求前缀
         *
         * @param url url
         * @return this
         * @author bootystar
         */
        public B baseUrl(String url) {
            if (url == null || url.isEmpty()) {
                this.getConfig().baseUrl = url;
                return this.getBuilder();
            }
            if (!url.startsWith("/")) {
                url = "/" + url;
            }
            if (url.endsWith("/")) {
                url = url.substring(0, url.length() - 1);
            }
            this.getConfig().baseUrl = url;
            return this.getBuilder();
        }

        /**
         * 跨域注解
         *
         * @return this
         * @author bootystar
         */
        public B enableCrossOrigins() {
            this.getConfig().crossOrigins = true;
            return this.getBuilder();
        }

        /**
         * 使用jakarta的api
         * (自springboot3起移除了javax包,使用jakarta替代)
         *
         * @return this
         * @author bootystar
         */
        public B enableJakartaApi() {
            this.getConfig().javaApiPackage = "jakarta";
            return this.getBuilder();
        }

        /**
         * 使用@AutoWired替换@Resource
         *
         * @return this
         * @author bootystar
         */
        public B enableAutoWired() {
            this.getConfig().autoWired = true;
            return this.getBuilder();
        }

        /**
         * 指定controller的返回结果包装类及方法
         *
         * @param methodReference 方法引用
         * @return {@link B }
         * @author bootystar
         */
        public <R> B returnMethod(SFunction<Object, R> methodReference) {
            this.getConfig().returnMethod = MybatisPlusReflectHelper.lambdaMethodInfo(methodReference, Object.class);
            return this.getBuilder();
        }

        /**
         * 指定controller返回的分页包装类及方法
         *
         * @param methodReference 方法参考
         * @return {@link B }
         * @author bootystar
         */
        public <O, R> B pageMethod(SFunction<IPage<O>, R> methodReference) {
            this.getConfig().pageMethod = MybatisPlusReflectHelper.lambdaMethodInfo(methodReference, IPage.class);
            return this.getBuilder();
        }

        /**
         * 禁止基础增删查改使用restful风格
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableRestful() {
            this.getConfig().restful = false;
            return this.getBuilder();
        }

        /**
         * 禁用消息体接收数据
         *
         * @return this
         * @author bootystar
         */
        public B disableRequestBody() {
            this.getConfig().requestBody = false;
            return this.getBuilder();
        }

        /**
         * 禁用参数校验注解
         *
         * @return this
         * @author bootystar
         */
        public B disableValidated() {
            this.getConfig().validated = false;
            return this.getBuilder();
        }

        /**
         * 禁止多条件复杂查询使用post请求
         *
         * @return this
         * @author bootystar
         */
        public B disablePostQuery() {
            this.getConfig().postQuery = false;
            return this.getBuilder();
        }

        //==================mapper=======================


        /**
         * 清空排序字段
         *
         * @return {@link B }
         * @author bootystar
         */
        public B sortColumnClear() {
            this.getConfig().orderColumnMap.clear();
            return this.getBuilder();
        }

        /**
         * 添加排序字段,越先添加优先级越高
         *
         * @param columnName 字段名
         * @param isDesc     是否倒排
         * @return this
         * @author bootystar
         */
        public B sortColumn(String columnName, boolean isDesc) {
            this.getConfig().orderColumnMap.put(columnName, isDesc);
            return this.getBuilder();
        }

        //============方法设置================

        /**
         * 不生成新增方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableInsert() {
            this.getConfig().generateInsert = false;
            return this.getBuilder();
        }

        /**
         * 不生成更新方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableUpdate() {
            this.getConfig().generateUpdate = false;
            return this.getBuilder();
        }

        /**
         * 不生成删除方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableDelete() {
            this.getConfig().generateDelete = false;
            return this.getBuilder();
        }

        /**
         * 不生成查询方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableSelect() {
            this.getConfig().generateSelect = false;
            return this.getBuilder();
        }

        /**
         * 不生成导入方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableImport() {
            this.getConfig().generateImport = false;
            return this.getBuilder();
        }

        /**
         * 不生成导出方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableExport() {
            this.getConfig().generateExport = false;
            return this.getBuilder();
        }


    }

}
