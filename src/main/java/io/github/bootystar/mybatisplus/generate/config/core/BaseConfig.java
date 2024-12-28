package io.github.bootystar.mybatisplus.generate.config.core;

import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhance.enums.SqlExtraSuffix;
import io.github.bootystar.mybatisplus.generate.info.ClassInfo;
import io.github.bootystar.mybatisplus.generate.info.MethodInfo;
import org.apache.ibatis.type.JdbcType;

import java.util.*;

/**
 * @author bootystar
 */
public abstract class BaseConfig {


    // ------------------生成相关配置----------------

    /**
     * 是否覆盖已有文件
     */
    protected boolean fileOverride;

    /**
     * 生成新增方法
     */
    protected boolean generateInsert = true;
    /**
     * 生成更新方法
     */
    protected boolean generateUpdate = true;
    /**
     * 生成删除方法
     */
    protected boolean generateDelete = true;
    /**
     * 生成查询方法
     */
    protected boolean generateSelect = true;
    /**
     * 生成导入方法
     */
    protected boolean generateImport = true;
    /**
     * 生成导出方法
     */
    protected boolean generateExport = true;

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
     * 请求基础url
     */
    protected String baseUrl;

    /**
     * 跨域注解
     */
    protected boolean crossOrigins;

    /**
     * javaEE api包(jakarta或javax)
     * <p>
     * 涉及HttpServletRequest,HttpServletResponse,@Resource
     */
    protected String javaApiPackage = "javax";

    /**
     * 使用@AutoWired替换@Resource
     */
    protected boolean autoWired;

    /**
     * 返回结果方法
     */
    protected MethodInfo returnMethod = new MethodInfo();

    /**
     * 分页结果方法
     */
    protected MethodInfo pageMethod = new MethodInfo();

    /**
     * restful样式
     */
    protected boolean restful = true;

    /**
     * controller是否使用@RequestBody注解
     */
    protected boolean requestBody = true;

    /**
     * 参数校验注解
     */
    protected boolean validated = true;

    /**
     * 复杂查询使用post请求
     */
    protected boolean postQuery = true;

    // ------------------mapper相关配置----------------

    /**
     * 排序字段map
     * 字段名 -> 是否倒序
     */
    protected Map<String, Boolean> orderColumnMap = new LinkedHashMap<>();

    // ------------------生成器运行时相关----------------

    /**
     * DTO及VO的导包
     */
    protected Set<String> importPackages4DTO;

    /**
     * 生成时间
     */
    protected String nowTime;

    /**
     * jdbc时间类型
     */
    protected List<JdbcType> jdbcTimeTypes;

    /**
     * mapper默认排序的sql语句
     */
    protected String orderBySql;

    /**
     * swagger注解的额外后缀uuid
     */
    protected String swaggerUUID;

    /**
     * 是否自定义了额外字段后缀
     */
    protected boolean extraFieldSuffixCustom;

    /**
     * 额外字段后缀映射
     */
    protected Map<String, String> extraFieldSuffixMap = SqlExtraSuffix.DEFAULT_MAP;

    // ------------------非通用相关----------------

    /**
     * 生成重写的父类方法
     */
    protected boolean overrideMethods = true;

    /**
     * 指定查询的DTO
     */
    protected ClassInfo selectDTO;

    /**
     * mapper入参dto
     */
    protected ClassInfo mapperDTO;

    /**
     * 额外字段后缀构建器
     */
    protected FieldSuffixBuilder extraFieldSuffixBuilder = new FieldSuffixBuilder();

}
