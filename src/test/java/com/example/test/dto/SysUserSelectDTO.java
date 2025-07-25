package com.example.test.dto;

import lombok.Data;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 用户-查询DTO
 *
 * @author bootystar
 * @since 2025-07-24
 * @see com.example.test.entity.SysUser
 * @see com.example.test.mapper.SysUserMapper
 */
@Data
public class SysUserSelectDTO {

    /**
     * id
     */
    private Long id;

    /**
     * id(IN)
     */
    private List<Long> idIn;

    /**
     * 版本号
     */
    private Integer version;

    /**
     * 版本号(IN)
     */
    private List<Integer> versionIn;

    /**
     * 版本号(>=)
     */
    private Integer versionGe;

    /**
     * 版本号(<=)
     */
    private Integer versionLe;

    /**
     * 创建时间
     */
    private LocalDateTime createTime;

    /**
     * 创建时间(>=)
     */
    private LocalDateTime createTimeGe;

    /**
     * 创建时间(<=)
     */
    private LocalDateTime createTimeLe;

    /**
     * 更新时间
     */
    private LocalDateTime updateTime;

    /**
     * 更新时间(>=)
     */
    private LocalDateTime updateTimeGe;

    /**
     * 更新时间(<=)
     */
    private LocalDateTime updateTimeLe;

    /**
     * 姓名
     */
    private String name;

    /**
     * 姓名Like后缀
     */
    private String nameLike;

    /**
     * 姓名Like后缀(LIKE)
     */
    private String nameLikeLike;

    public String getNameLikeLike() {
        return nameLikeLike == null || nameLikeLike.isEmpty() ? null : "%" + nameLikeLike + "%";
    }

    /**
     * 年龄
     */
    private Integer age;

    /**
     * 年龄(IN)
     */
    private List<Integer> ageIn;

    /**
     * 年龄(>=)
     */
    private Integer ageGe;

    /**
     * 年龄(<=)
     */
    private Integer ageLe;

    /**
     * 生日
     */
    private LocalDate birthDate;

    /**
     * 生日(IN)
     */
    private List<LocalDate> birthDateIn;

    /**
     * 生日(>=)
     */
    private LocalDate birthDateGe;

    /**
     * 生日(<=)
     */
    private LocalDate birthDateLe;

    /**
     * 状态
     */
    private Integer state;

    /**
     * 状态(IN)
     */
    private List<Integer> stateIn;

    /**
     * 状态(>=)
     */
    private Integer stateGe;

    /**
     * 状态(<=)
     */
    private Integer stateLe;
}
