package com.example.test.dto;

import cn.idev.excel.annotation.ExcelIgnoreUnannotated;
import cn.idev.excel.annotation.ExcelProperty;
import lombok.Data;

import java.time.LocalDate;

/**
 * 用户-新增DTO
 *
 * @author bootystar
 * @since 2025-07-24
 * @see com.example.test.entity.SysUser
 */
@Data
@ExcelIgnoreUnannotated
public class SysUserInsertDTO {

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
