package com.example.test.dto;

import lombok.Data;

import java.time.LocalDate;

/**
 * 用户-修改DTO
 *
 * @author bootystar
 * @since 2025-07-24
 * @see com.example.test.entity.SysUser
 */
@Data
public class SysUserUpdateDTO {

    /**
     * id
     */
    private Long id;

    /**
     * 版本号
     */
    private Integer version;

    /**
     * 姓名
     */
    private String name;

    /**
     * 姓名Like后缀
     */
    private String nameLike;

    /**
     * 年龄
     */
    private Integer age;

    /**
     * 生日
     */
    private LocalDate birthDate;

    /**
     * 状态
     */
    private Integer state;

}
