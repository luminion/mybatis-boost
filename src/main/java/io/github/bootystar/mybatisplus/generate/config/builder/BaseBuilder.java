package io.github.bootystar.mybatisplus.generate.config.builder;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import io.github.bootystar.mybatisplus.generate.config.core.BaseConfig;
import io.github.bootystar.mybatisplus.generate.config.core.CustomConfig;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;
import lombok.SneakyThrows;

import java.io.File;
import java.util.Arrays;

/**
 * @author bootystar
 */
@SuppressWarnings("unused")
public abstract class BaseBuilder<B extends BaseBuilder<B>> extends BaseConfig {

    /**
     * 获取构建器
     *
     * @return this
     */
    @SuppressWarnings("unchecked")
    protected B getBuilder() {
        return (B) this;
    }

    /**
     * 创建配置
     *
     * @return 模板配置对象
     */
    public CustomConfig build() {
        return new CustomConfig(this);
    }

    // ------------------生成相关配置----------------

    /**
     * 开启文件覆盖(vo及dto)
     *
     * @return this
     */
    public B enableFileOverride() {
        this.fileOverride = true;
        return this.getBuilder();
    }

    /**
     * 不生成新增方法
     *
     * @return this
     */
    public B disableInsert() {
        this.generateInsert = false;
        return this.getBuilder();
    }

    /**
     * 不生成更新方法
     *
     * @return this
     */
    public B disableUpdate() {
        this.generateUpdate = false;
        return this.getBuilder();
    }

    /**
     * 不生成删除方法
     *
     * @return this
     */
    public B disableDelete() {
        this.generateDelete = false;
        return this.getBuilder();
    }

    /**
     * 不生成查询方法
     *
     * @return this
     */
    public B disableSelect() {
        this.generateSelect = false;
        return this.getBuilder();
    }

    /**
     * 不生成导入方法
     *
     * @return this
     */
    public B disableImport() {
        this.generateImport = false;
        return this.getBuilder();
    }

    /**
     * 不生成导出方法
     *
     * @return this
     */
    public B disableExport() {
        this.generateExport = false;
        return this.getBuilder();
    }

    /**
     * 禁用swagger/springdoc模型实体的注解
     * <p>
     * 已知swagger注解在同名时有冲突, 禁用后请确保表注释不为空且不同名
     *
     * @return this
     */
    public B enableSwaggerModelWithAnnotation() {
        this.swaggerModelWithAnnotation = true;
        return this.getBuilder();
    }

    /**
     * 禁用swagger/springdoc文档额外uuid标识
     * <p>
     * 已知swagger注解在同名时有冲突, 禁用后请确保表注释不为空且不同名
     *
     * @return this
     */
    public B enableSwaggerAnnotationWithUUID() {
        this.swaggerAnnotationWithUUID = true;
        return this.getBuilder();
    }

    //------------------额外类相关配置----------------

    /**
     * DTO所在包
     *
     * @param packageName 包名
     * @return this
     */
    public B package4DTO(String packageName) {
        this.package4DTO = packageName;
        return this.getBuilder();
    }

    /**
     * DTO文件输出路径
     *
     * @param path 路径
     * @return this
     */
    @SneakyThrows
    public B path4DTO(String path) {
        this.path4DTO = new File(path).getCanonicalPath();
        return this.getBuilder();
    }

    /**
     * VO所在包
     *
     * @param packageName 包名
     * @return this
     */
    public B package4VO(String packageName) {
        this.package4VO = packageName;
        return this.getBuilder();
    }

    /**
     * DTO文件输出路径
     *
     * @param path 路径
     * @return this
     */
    @SneakyThrows
    public B path4VO(String path) {
        this.path4VO = new File(path).getCanonicalPath();
        return this.getBuilder();
    }

    /**
     * 新增或修改时排除的字段
     *
     * @param columns 字段名称
     * @return this
     */
    public B editExcludeColumns(String... columns) {
        this.editExcludeColumns = Arrays.asList(columns);
        return this.getBuilder();
    }

    // ------------------controller相关配置----------------

    /**
     * controller请求前缀
     *
     * @param url url
     * @return this
     */
    public B baseUrl(String url) {
        if (url == null || url.isEmpty()) {
            this.baseUrl = url;
            return this.getBuilder();
        }
        if (!url.startsWith("/")) {
            url = "/" + url;
        }
        if (url.endsWith("/")) {
            url = url.substring(0, url.length() - 1);
        }
        this.baseUrl = url;
        return this.getBuilder();
    }

    /**
     * 跨域注解
     *
     * @return this
     */
    public B enableCrossOrigins() {
        this.crossOrigins = true;
        return this.getBuilder();
    }

    /**
     * 使用jakarta的api(springboot3及以上开启)
     *
     * @return this
     */
    public B enableJakartaApi() {
        this.javaApiPackage = "jakarta";
        return this.getBuilder();
    }

    /**
     * 使用@AutoWired替换@Resource
     *
     * @return this
     */
    public B enableAutoWired() {
        this.autoWired = true;
        return this.getBuilder();
    }

    /**
     * 多条件复杂查询使用post请求(使用Get请求替代)
     *
     * @return this
     */
    public B enablePostQuery() {
        this.postQuery = true;
        return this.getBuilder();
    }

    /**
     * 指定controller的返回结果包装类及方法
     *
     * @param methodReference 方法引用
     * @return this
     */
    public <R> B returnMethod(SFunction<Object, R> methodReference) {
        this.returnMethod = MybatisPlusReflectHelper.lambdaMethodInfo(methodReference, Object.class);
        return this.getBuilder();
    }

    /**
     * 指定controller返回的分页包装类及方法
     *
     * @param methodReference 方法参考
     * @return this
     */
    public <O, R> B pageMethod(SFunction<IPage<O>, R> methodReference) {
        this.pageMethod = MybatisPlusReflectHelper.lambdaMethodInfo(methodReference, IPage.class);
        return this.getBuilder();
    }

    /**
     * 禁止基础增删查改使用restful风格
     *
     * @return this
     */
    public B disableRestful() {
        this.restful = false;
        return this.getBuilder();
    }

    /**
     * 禁用消息体接收数据
     *
     * @return this
     */
    public B disableRequestBody() {
        this.requestBody = false;
        return this.getBuilder();
    }

    /**
     * 禁用参数校验注解
     *
     * @return this
     */
    public B disableValidated() {
        this.validated = false;
        return this.getBuilder();
    }

    // ------------------mapper相关配置----------------

    /**
     * 清空排序字段
     *
     * @return this
     */
    public B sortColumnClear() {
        this.orderColumnMap.clear();
        return this.getBuilder();
    }

    /**
     * 添加排序字段,越先添加优先级越高
     *
     * @param columnName 字段名
     * @param isDesc     是否倒排
     * @return this
     */
    public B sortColumn(String columnName, boolean isDesc) {
        this.orderColumnMap.put(columnName, isDesc);
        return this.getBuilder();
    }

}
