package io.github.bootystar.mybatisplus.generate.config.builder;

import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.UnmodifiableSqlHelper;
import io.github.bootystar.mybatisplus.generate.info.ClassInfo;
import lombok.Getter;

/**
 * @author bootystar
 */
@Getter
public class DynamicSqlBuilder extends BaseEnhanceBuilder<DynamicSqlBuilder> {

    {
        selectDTO = new ClassInfo(SqlHelper.class);
        mapperDTO = new ClassInfo(UnmodifiableSqlHelper.class);
    }

    @Override
    public DynamicSqlBuilder disableOverrideMethods() {
        return super.disableOverrideMethods();
    }

    @Override
    public DynamicSqlBuilder withMapSelectDTO() {
        return super.withMapSelectDTO();
    }

}


