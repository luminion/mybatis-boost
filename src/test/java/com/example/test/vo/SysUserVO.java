package com.example.test.vo;

import cn.idev.excel.annotation.ExcelIgnoreUnannotated;
import cn.idev.excel.annotation.ExcelProperty;
import lombok.Data;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 用户-VO
 *
 * @author bootystar
 * @since 2025-07-24
 * @see com.example.test.entity.SysUser
 * @see com.example.test.mapper.SysUserMapper
 */
@Data
@ExcelIgnoreUnannotated
public class SysUserVO {

    /**
     * id
     */
    @ExcelProperty(value = "id")
    private Long id;

    /**
     * 版本号
     */
    @ExcelProperty(value = "版本号")
    private Integer version;

    /**
     * 创建时间
     */
    @ExcelProperty(value = "创建时间")
    private LocalDateTime createTime;

    /**
     * 更新时间
     */
    @ExcelProperty(value = "更新时间")
    private LocalDateTime updateTime;

    /**
     * 姓名
     */
    @ExcelProperty(value = "姓名")
    private String name;

    /**
     * 姓名Like后缀
     */
    @ExcelProperty(value = "姓名Like后缀")
    private String nameLike;

    /**
     * 年龄
     */
    @ExcelProperty(value = "年龄")
    private Integer age;

    /**
     * 生日
     */
    @ExcelProperty(value = "生日")
    private LocalDate birthDate;

    /**
     * 状态
     */
    @ExcelProperty(value = "状态")
    private Integer state;
}
